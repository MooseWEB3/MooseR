# Platform-independent removal of accents. iconv(x, to = "ASCII//TRANSLIT")
# depends on the system's iconv: macOS turns "e acute" into "'e", splitting
# words, while Windows and glibc give "e". This mapping gives the same result
# everywhere for Latin-1 and Latin Extended-A letters (U+00C0 to U+017F);
# ligatures are spelled out and any other non-ASCII character is dropped.
.moose_fold_from <- paste0(
    "\u00c0\u00c1\u00c2\u00c3\u00c4\u00c5\u00c7\u00c8\u00c9\u00ca\u00cb\u00cc",
    "\u00cd\u00ce\u00cf\u00d0\u00d1\u00d2\u00d3\u00d4\u00d5\u00d6\u00d8\u00d9",
    "\u00da\u00db\u00dc\u00dd\u00e0\u00e1\u00e2\u00e3\u00e4\u00e5\u00e7\u00e8",
    "\u00e9\u00ea\u00eb\u00ec\u00ed\u00ee\u00ef\u00f0\u00f1\u00f2\u00f3\u00f4",
    "\u00f5\u00f6\u00f8\u00f9\u00fa\u00fb\u00fc\u00fd\u00ff\u0100\u0101\u0102",
    "\u0103\u0104\u0105\u0106\u0107\u0108\u0109\u010a\u010b\u010c\u010d\u010e",
    "\u010f\u0110\u0111\u0112\u0113\u0114\u0115\u0116\u0117\u0118\u0119\u011a",
    "\u011b\u011c\u011d\u011e\u011f\u0120\u0121\u0122\u0123\u0124\u0125\u0126",
    "\u0127\u0128\u0129\u012a\u012b\u012c\u012d\u012e\u012f\u0130\u0131\u0134",
    "\u0135\u0136\u0137\u0138\u0139\u013a\u013b\u013c\u013d\u013e\u013f\u0140",
    "\u0141\u0142\u0143\u0144\u0145\u0146\u0147\u0148\u014a\u014b\u014c\u014d",
    "\u014e\u014f\u0150\u0151\u0154\u0155\u0156\u0157\u0158\u0159\u015a\u015b",
    "\u015c\u015d\u015e\u015f\u0160\u0161\u0162\u0163\u0164\u0165\u0166\u0167",
    "\u0168\u0169\u016a\u016b\u016c\u016d\u016e\u016f\u0170\u0171\u0172\u0173",
    "\u0174\u0175\u0176\u0177\u0178\u0179\u017a\u017b\u017c\u017d\u017e\u017f"
)
.moose_fold_to <- paste0(
    "AAAAAACEEEEI",
    "IIIDNOOOOOOU",
    "UUUYaaaaaace",
    "eeeiiiidnooo",
    "ooouuuuyyAaA",
    "aAaCcCcCcCcD",
    "dDdEeEeEeEeE",
    "eGgGgGgGgHhH",
    "hIiIiIiIiIiJ",
    "jKkkLlLlLlLl",
    "LlNnNnNnNnOo",
    "OoOoRrRrRrSs",
    "SsSsSsTtTtTt",
    "UuUuUuUuUuUu",
    "WwYyYZzZzZzs"
)
.moose_fold_multi <- c(
    "\u00c6" = "AE",
    "\u00de" = "TH",
    "\u00df" = "ss",
    "\u00e6" = "ae",
    "\u00fe" = "th",
    "\u0132" = "IJ",
    "\u0133" = "ij",
    "\u0149" = "n",
    "\u0152" = "OE",
    "\u0153" = "oe"
)

moose_ascii_fold <- function(x) {
  out <- enc2utf8(as.character(x))
  for (letter in names(.moose_fold_multi)) {
    out <- gsub(letter, .moose_fold_multi[[letter]], out, fixed = TRUE)
  }
  out <- chartr(.moose_fold_from, .moose_fold_to, out)
  iconv(out, from = "UTF-8", to = "ASCII", sub = "")
}
