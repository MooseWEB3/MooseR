"""Refresh the Alberta hospital snapshot from AHS (Python 3, standard library).

Run from any directory, then run data-raw/create-alberta-hospitals.R from the
package root. No API key is needed.
See alberta-hospitals-sources.txt for the inclusion rules and source terms.
"""

import argparse
import csv
from datetime import datetime
from html.parser import HTMLParser
from pathlib import Path
import re
import subprocess
import tempfile
import time
from urllib.parse import parse_qs, urljoin, urlsplit
from zoneinfo import ZoneInfo


BASE = "https://www.albertahealthservices.ca/findhealth/"
DIRECTORY = BASE + "service.aspx?Id=1001416"
PSYCHIATRIC_SOURCE = (
    "https://www.alberta.ca/"
    "detention-treatment-and-care-while-in-a-mental-health-facility"
)
SPECIALIST_SOURCE = "https://www.albertahealthservices.ca/hf/Page18051.aspx"
# These inpatient specialist facilities are absent from the Hospitals service
# list. Names are checked on refresh so a reassigned/retired ID cannot slip in.
SUPPLEMENT = [
    ("6337", "Alberta Hospital Edmonton", PSYCHIATRIC_SOURCE),
    ("1000242", "Centennial Centre for Mental Health and Brain Injury", PSYCHIATRIC_SOURCE),
    ("1016451", "Claresholm Centre for Mental Health & Addictions", PSYCHIATRIC_SOURCE),
    ("1007343", "Southern Alberta Forensic Psychiatry Centre", PSYCHIATRIC_SOURCE),
    ("1044105", "Villa Caritas", PSYCHIATRIC_SOURCE),
    ("1024456", "Lois Hole Hospital for Women", SPECIALIST_SOURCE),
    ("1024455", "Mazankowski Alberta Heart Institute", SPECIALIST_SOURCE),
    ("6122", "Cross Cancer Institute", "https://www.albertahealthservices.ca/cci/cci.aspx"),
]
FIELDS = [
    "facility_id", "hospital_name", "address", "street_address", "city",
    "province", "postal_code", "latitude", "longitude", "inclusion_basis",
    "inclusion_source_url", "source_url", "retrieved_on", "emergency_24h",
]


class Page(HTMLParser):
    """Read labelled AHS fields and links without executing page scripts."""

    def __init__(self, html):
        super().__init__(convert_charrefs=True)
        self.labels = {}
        self.links = []
        self.active = None
        self.feed(html)

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if tag in ("span", "a") and self.active is None:
            self.active = {"tag": tag, "attrs": attrs, "text": []}
        elif tag == "br" and self.active is not None:
            self.active["text"].append(" ")

    def handle_data(self, data):
        if self.active is not None:
            self.active["text"].append(data)

    def handle_endtag(self, tag):
        if self.active is None or tag != self.active["tag"]:
            return
        attrs = self.active["attrs"]
        text = " ".join("".join(self.active["text"]).split())
        if "id" in attrs:
            if attrs["id"] in self.labels:
                raise ValueError("Duplicate page field: " + attrs["id"])
            self.labels[attrs["id"]] = text
        if tag == "a" and "href" in attrs:
            self.links.append((attrs, text))
        self.active = None


def fetch(url):
    # curl uses the operating system's certificate store on macOS/Windows.
    result = subprocess.run(
        ["curl", "--fail", "--location", "--silent", "--show-error",
         "--retry", "3", "--max-time", "60", url],
        check=True, capture_output=True, encoding="utf-8",
    )
    time.sleep(0.15)
    return result.stdout


def required(page, field):
    value = page.labels.get(field, "")
    if not value:
        raise ValueError("Missing AHS page field: " + field)
    return value


def add_reviewed_emergency_flags(rows):
    """Join the separately dated ED review; never infer ED hours from hospital hours.

    Review/update this evidence file separately when refreshing ED schedules.
    Unknown facilities or changed names stop the refresh instead of becoming 0.
    """
    path = Path(__file__).resolve().parents[1] / "inst/extdata/alberta-hospitals-emergency.csv"
    with path.open(encoding="utf-8", newline="") as handle:
        evidence = list(csv.DictReader(handle))
    indexed = {row["facility_id"]: row for row in evidence}
    if len(indexed) != len(evidence):
        raise ValueError("Duplicate facility ID in emergency evidence")
    if set(indexed) != {row["facility_id"] for row in rows}:
        raise ValueError("Hospital membership changed; review the emergency evidence first")
    for row in rows:
        item = indexed.get(row["facility_id"])
        if item is None or item["hospital_name"] != row["hospital_name"]:
            raise ValueError("ED schedule review required: " + row["hospital_name"])
        flag = item["emergency_24h"]
        if flag not in ("0", "1") or not item["source_url"] or not item["checked_on"]:
            raise ValueError("Invalid emergency evidence: " + row["hospital_name"])
        if item["assessment"] not in ("24h_ed", "limited_hours_ed", "no_ed"):
            raise ValueError("Unknown ED assessment: " + row["hospital_name"])
        if (flag == "1") != (item["assessment"] == "24h_ed"):
            raise ValueError("Contradictory ED evidence: " + row["hospital_name"])
        row["emergency_24h"] = flag
    return rows


def record(html, url, facility_id, expected_name, basis, inclusion_url, date):
    page = Page(html)
    name_field = ("MainPlaceHolder_facilityContactOfficialNameLabel"
                  if "service.aspx" in url.lower() else "officialNameLabel")
    name = required(page, name_field)
    if name != expected_name:
        raise ValueError(f"Hospital name changed: {expected_name!r} -> {name!r}")
    street = required(page, "MainPlaceHolder_addressLabel")
    city, province = required(page, "MainPlaceHolder_cityProvinceLabel").rsplit(", ", 1)
    postal = required(page, "MainPlaceHolder_postalCodeLabel").upper()
    if province != "Alberta":
        if province != "Saskatchewan" or "Lloydminster" not in name:
            raise ValueError(f"Unexpected province: {name}: {province}")
        print(f"Excluded (outside Alberta): {name}", flush=True)
        return None
    coords = re.findall(
        r"mainlocation\.push\(\[\s*'(-?\d+(?:\.\d+)?)'\s*,\s*'(-?\d+(?:\.\d+)?)'",
        html,
    )
    if len(coords) != 1:
        raise ValueError(f"Expected one official map coordinate: {name}")
    lat, lon = map(float, coords[0])
    if not (49 <= lat <= 60 and -120 <= lon <= -110):
        raise ValueError(f"Coordinates outside Alberta bounds: {name}: {coords[0]}")
    if not re.fullmatch(r"T\d[A-Z] \d[A-Z]\d", postal):
        raise ValueError(f"Unexpected Alberta postal code: {name}: {postal}")
    return dict(zip(FIELDS, [
        facility_id, name, f"{street}, {city}, {province} {postal}", street,
        city, province, postal, lat, lon, basis, inclusion_url, url, date,
    ]))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--date", default=datetime.now(ZoneInfo("America/Edmonton")).date().isoformat())
    args = parser.parse_args()
    datetime.strptime(args.date, "%Y-%m-%d")
    directory = Page(fetch(DIRECTORY))
    locations = []
    for attrs, name in directory.links:
        url = urljoin(BASE, attrs["href"]).split("#")[0]
        query = {k.lower(): v for k, v in parse_qs(urlsplit(url).query).items()}
        if query.get("id") == ["1001416"] and "serviceatfacilityid" in query:
            locations.append((url, name))
    if len(locations) < 100 or len(set(url for url, _ in locations)) != len(locations):
        raise ValueError("Incomplete or duplicated Hospitals service directory")
    rows = []
    for index, (url, name) in enumerate(locations, 1):
        html = fetch(url)
        page = Page(html)
        facility_links = [a["href"] for a, _ in page.links
                          if a.get("id") == "facilityOfficialNameHyperLink"]
        if len(facility_links) != 1:
            raise ValueError("Missing facility link: " + name)
        facility_id = parse_qs(urlsplit(facility_links[0]).query)["id"][0]
        row = record(html, url, facility_id, name, "ahs_hospitals_service", DIRECTORY, args.date)
        if row is not None:
            rows.append(row)
        if index % 20 == 0:
            print(f"Read {index}/{len(locations)} hospital service locations", flush=True)
    for facility_id, name, inclusion_url in SUPPLEMENT:
        if any(row["facility_id"] == facility_id for row in rows):
            continue
        url = BASE + "facility.aspx?id=" + facility_id
        row = record(fetch(url), url, facility_id, name, "specialist_supplement", inclusion_url, args.date)
        if row is None:
            raise ValueError("Specialist supplement is outside Alberta: " + name)
        rows.append(row)
    for field in ("facility_id", "hospital_name"):
        if len({row[field] for row in rows}) != len(rows):
            raise ValueError("Duplicate " + field)
    rows.sort(key=lambda row: row["hospital_name"].casefold())
    rows = add_reviewed_emergency_flags(rows)
    target = Path(__file__).resolve().parents[1] / "inst/extdata/alberta-hospitals.csv"
    # Preserve the previous complete snapshot if any request or check fails.
    with tempfile.NamedTemporaryFile(mode="w", encoding="utf-8", newline="",
                                     dir=target.parent, delete=False) as handle:
        writer = csv.DictWriter(handle, fieldnames=FIELDS, lineterminator="\n")
        writer.writeheader()
        writer.writerows(rows)
        temporary = Path(handle.name)
    temporary.replace(target)
    print(f"Saved {len(rows)} hospitals to {target}")


if __name__ == "__main__":
    main()
