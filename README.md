# MooseR

[English](#english) | [Français](#francais) | [Português](#portugues)

<a id="english"></a>

## English

MooseR is a small R utility package for common data-cleaning and summary-table
tasks. It includes helpers for column-name cleaning, duplicate detection,
uniqueness checks, quote removal, one-row summaries for categorical and
continuous variables, covariate balance comparison, stable de-identified SID
generation, and date formatting.

The core data-cleaning helpers are intentionally lightweight. The personal-name
masking workflow can use `reticulate` plus spaCy when Python is available, but it
also has a pure R regex fallback for locked-down work computers where Python
cannot be changed. MooseR requires R 4.1.0 or later.

### Installation

Install the development version from GitHub:

```r
install.packages("remotes")
remotes::install_github("MooseWEB3/MooseR", upgrade = "never")
```

Load the package:

```r
library(MooseR)
```

If you need to reinstall the latest GitHub version over an existing local copy:

```r
remotes::install_github("MooseWEB3/MooseR", force = TRUE, upgrade = "never")
```

### Startup package loading

Load the default MooseR workflow package set in the current R session:

```r
Moose_load_mooser_packages()
```

The default set is:

```r
Moose_default_mooser_packages()
```

To make R or RStudio load those packages automatically every time it starts,
run this once:

```r
Moose_enable_mooser_startup_packages()
```

This writes a MooseR-managed block to `~/.Rprofile`. Restart R or RStudio after
enabling it. To remove the automatic startup loading later:

```r
Moose_disable_mooser_startup_packages()
```

### Functions

MooseR keeps its original function names for backward compatibility. New code
should use the `Moose_`-prefixed names. Older names remain available so
existing scripts do not break.

| Recommended function | Older compatible name | Purpose |
| --- | --- | --- |
| `Moose_fix_colname()` | `BD_fix_colname()` | Clean column names by replacing spaces and removing selected special characters. |
| `Moose_get_duplicates()` | `BD_get_duplicates()` | Find duplicated rows by one or more key columns. |
| `Moose_check_unique()` | `BD_check_unique()` | Check whether a column contains unique values. |
| `Moose_count_unique_categories()` | `BD_count_unique_categories()` | Count unique categories in a variable. |
| `Moose_quote_rm()` | `BD_quote_rm()` | Remove common quote characters from character and factor columns. |
| `Moose_1_cat()` | `BD_1_cat()` | Build a one-row count and percentage table for a categorical variable. |
| `Moose_1_cont()` | `BD_1_cont()` | Build a one-row summary table for a continuous variable. |
| `Moose_compare_balance()` | `BD_compare_balance()` | Compare covariate balance between two data frames. |
| `Moose_SID_creator()` | `BD_SID_creator()` | Create a stable de-identified SID from name, date of birth, and gender. |
| `Moose_today()` | `BD_today()` | Return today's date in underscore and compact formats. |
| `Moose_read_csv()` | - | Read a CSV file and clean its column names. |
| `Moose_SRds()` | - | Save an R object to an RDS file. |
| `Moose_LRds()` | - | Restore an R object from an RDS file. |
| `Moose_setup_name_masking()` | `setup_name_masking()` | Prepare the spaCy engine when available, or the pure R regex fallback. |
| `Moose_check_name_masking()` | `check_name_masking()` | Report the current Python, spaCy, and model setup status. |
| `Moose_mask_person_names()` | `mask_person_names()` | Replace detected personal names in text with a replacement string. |
| `Moose_detect_person_names()` | `detect_person_names()` | Return an audit table of detected names and character offsets. |
| `Moose_name_flag()` | - | Return a 1/0 vector showing whether each text value contains a detected personal name. |
| `Moose_apply_name_masking_rules()` | `apply_name_masking_rules()` | Apply supplementary regex-based name-masking rules. |
| `Moose_todate()` | - | Convert common date inputs to R `Date` values. |
| `Moose_todatetime()` | - | Convert common date and date-time inputs to R `POSIXct` values. |
| `Moose_boost_data()` | - | Detect and convert date-like columns in a data set. |
| `Moose_load_packages()` | `load_packages()` | Install missing packages and load a package list. |
| `Moose_load_mooser_packages()` | `load_mooser_packages()` | Load the default MooseR startup package set. |
| `Moose_enable_mooser_startup_packages()` | `enable_mooser_startup_packages()` | Enable automatic package loading at R startup. |
| `Moose_disable_mooser_startup_packages()` | `disable_mooser_startup_packages()` | Disable automatic package loading at R startup. |

### Examples

Clean column names:

```r
df <- data.frame("Col (1)" = 1:3, "Name's-Age" = c(20, 25, 30))
Moose_fix_colname(df)
```

Summarize a categorical variable:

```r
df <- data.frame(group = c("A", "B", "A", NA, "C", "B", "B"))
Moose_1_cat(df, "group", display_name = "Group")
```

Summarize a continuous variable:

```r
df <- data.frame(age = c(21, 34, 45, 52, NA, 63))
Moose_1_cont(df, "age", display_name = "Age")
```

Find duplicate rows by key:

```r
Moose_get_duplicates(mtcars, cyl, gear, .keep_all = FALSE)
```

Check uniqueness:

```r
Moose_check_unique(iris, Species)
Moose_check_unique(iris, "Species", na.rm = TRUE)
```

Create stable de-identified SIDs:

```r
Moose_SID_creator(
  first_name = c("Ann", "Marie"),
  last_name = c("Martin", "Dubois"),
  DOB_ready = c("1990-07-03", "2000/01/02"),
  gender = c("F", "M")
)
```

Compare balance between two data frames:

```r
set.seed(1)
a <- data.frame(x = rnorm(100), g = sample(c("M", "F"), 100, TRUE))
b <- data.frame(x = rnorm(120, 0.2, 1.1), g = sample(c("M", "F"), 120, TRUE))

Moose_compare_balance(a, b, vars = c("x", "g"), include_smd = TRUE)
```

Get today's date:

```r
Moose_today()
```

Convert common date and date-time inputs:

```r
Moose_todate(c("2024-01-05", "01/06/2024", "20240107"))
Moose_todate(c(20240105, 45296))

Moose_todatetime(c("2024-01-05 13:30:00", "2024/01/06 8:05"))
Moose_todatetime(c(202401051330, 1704450600))
```

Automatically convert date-like columns in a data set:

```r
raw_data <- data.frame(
  visit_date = c("2024-01-05", "2024/01/06"),
  created_at = c("2024-01-05 13:30", "2024-01-06 08:05"),
  note = c("First visit", "Follow-up")
)

boosted_data <- Moose_boost_data(raw_data)
str(boosted_data)
```

Save and restore an R object in RDS format:

```r
Moose_SRds(boosted_data, "boosted_data")
restored_data <- Moose_LRds("boosted_data")
```

Set up personal-name masking. On normal computers, `engine = "auto"` tries spaCy
and falls back to regex if Python cannot be initialized:

```r
Moose_setup_name_masking()
Moose_check_name_masking()
```

On locked-down work computers where Python cannot be changed, use the pure R
engine directly:

```r
Moose_setup_name_masking(engine = "regex")
Moose_check_name_masking()
```

In regex mode, `Moose_check_name_masking()` should show `Engine: regex`. Python,
spaCy, and the spaCy model are not used in that mode.

The regex engine ignores standalone all-uppercase phrases such as
`MEDICAL RECORDS` or `THE PATIENT`. It accepts all-uppercase names only after a
supported title, such as `RN SMITH`, or a workflow phrase, such as
`Reviewed by JOHN SMITH`.
Healthcare organization names ending in words such as `Hospital`, `Clinic`,
`Centre`, `Health`, or `Foundation` are not treated as personal names. For
example, `St Mary Hospital` and `Vancouver General Hospital` are preserved.
Clinical phrases containing words such as `Chest`, `Pain`, `Disease`, or
`Syndrome` are also preserved; for example, `Cardia Chest Pain` is not masked.
Treatment phrases such as `Normal Saline IV` are also preserved.
Clinical document labels such as `RN Report` are also preserved.
The built-in medical whitelist also preserves title-cased phrases such as
`Chief Complaint`, `Present Illness`, `Code Status`, `On Arrival`,
`Interfacility Transfer`, `Transport Truck`, `Ground Level`, `Nursing Home`, `Altered Mental Status`,
`Nausea Vomiting`, `Nose Bleed`, `Dog Bite`, `Urinary Issues`, `Patient Demographics`,
`Behavioural Problems`, `Substance Abuse`, `Heart Transplant`, `Seizure Activity`,
and `Opioid Overdose`.
This whitelist intentionally takes precedence over the title-case name pattern;
audit data where a real person's name could contain one of these terms.
Location phrases such as `Private Residence`, `West Side`, `Costco Store`, and
`Edmonton International Airport`,
and clinical states such as `Pt Syncopal` and `Heavily Intoxicated`, are also
preserved.
These exclusions are applied to both the regex and spaCy engines.
MooseR also preserves all 334 official Alberta municipality names, including
`Medicine Hat`, `Red Deer`, and `Rocky Mountain House`. The registry follows
the Government of Alberta 2024 Municipal Codes and excludes the administrative
`Special Areas Board`. The Alberta place name `Fort McMurray` and the common
spelling `Fort McMurry` are also preserved.
MooseR also excludes the 1,150 unique facility names in the Alberta Health
Services [Find Healthcare directory](https://www.albertahealthservices.ca/findhealth/search.aspx),
including hospitals, health centres, clinics, continuing-care sites, and other
official service locations. The complete facility name must occur in the text:
`Peter Lougheed Centre`, `Grand Manor`, and `Chartwell Griesbach` are preserved,
while a standalone personal name is still detected.
MooseR also excludes all 2,616 unique school names in the Alberta Education
[School Information Report](https://education.alberta.ca/alberta-education/school-authority-index/),
covering public, separate, Francophone, charter, independent, First Nations,
ECS private-operator, and provincial schools. Matching is case-insensitive, and
the complete school name must occur in the text. A person's name that is also
part of a school name is still detected when the full school name is absent.

Mask names in a character vector:

```r
comments <- c(
  "John Smith spoke with Sarah Johnson in Vancouver.",
  "The patient was transported to Vancouver.",
  "Reviewed by Michael Brown.",
  "",
  NA
)

Moose_mask_person_names(comments)
```

Force the pure R regex engine:

```r
Moose_mask_person_names(comments, engine = "regex")
```

Keep the original and masked text side by side:

```r
Moose_mask_person_names(comments, keep_original = TRUE)
```

Create an audit table of detected names:

```r
Moose_detect_person_names(comments)
```

Add a row-level flag for detected personal names:

```r
records <- data.frame(comments = comments)
flagged_records <- dplyr::mutate(
  records,
  name_flag = Moose_name_flag(comments)
)
flagged_records
```

For large data sets, remove grouping first so the complete column is processed
in one call:

```r
flagged_records <- records |>
  dplyr::ungroup() |>
  dplyr::mutate(
    name_flag = Moose_name_flag(comments, batch_size = 2000L)
  )
```

Use supplementary regex rules after spaCy masking:

```r
masked <- Moose_mask_person_names(comments)
Moose_apply_name_masking_rules(masked)
```

For real medical or privacy-sensitive data, keep the audit table and
review a sample of the output. Named-entity recognition is helpful, but it is
not a guarantee that every personal name is found.

### License

MIT license. Copyright (c) 2026 LisbonBulldog.

---

<a id="francais"></a>

## Français

MooseR est un petit package utilitaire R destiné aux tâches courantes de
nettoyage des données et de création de tableaux récapitulatifs. Il comprend
des fonctions pour nettoyer les noms de colonnes, détecter les doublons,
vérifier l'unicité, supprimer les guillemets, produire des résumés sur une
seule ligne pour les variables catégorielles et continues, comparer
l'équilibre des covariables, générer des identifiants SID stables et
désidentifiés, et convertir les dates.

Les principales fonctions de nettoyage des données sont volontairement
légères. Le processus de masquage des noms de personnes peut utiliser
`reticulate` et spaCy lorsque Python est disponible. Il comprend également une
solution de rechange entièrement en R utilisant des expressions régulières
pour les ordinateurs de travail verrouillés où Python ne peut pas être
modifié. MooseR nécessite R 4.1.0 ou une version ultérieure.

### Installation

Installez la version de développement depuis GitHub :

```r
install.packages("remotes")
remotes::install_github("MooseWEB3/MooseR", upgrade = "never")
```

Chargez le package :

```r
library(MooseR)
```

Pour réinstaller la dernière version GitHub par-dessus une copie locale
existante :

```r
remotes::install_github("MooseWEB3/MooseR", force = TRUE, upgrade = "never")
```

### Chargement des packages au démarrage

Chargez l'ensemble de packages MooseR par défaut dans la session R actuelle :

```r
Moose_load_mooser_packages()
```

Pour afficher la liste par défaut :

```r
Moose_default_mooser_packages()
```

Pour que R ou RStudio charge automatiquement ces packages à chaque démarrage,
exécutez cette commande une seule fois :

```r
Moose_enable_mooser_startup_packages()
```

Cette commande ajoute un bloc géré par MooseR dans `~/.Rprofile`. Redémarrez R
ou RStudio après l'activation. Pour désactiver ultérieurement le chargement
automatique :

```r
Moose_disable_mooser_startup_packages()
```

### Fonctions

MooseR conserve les noms de fonctions d'origine pour assurer la
rétrocompatibilité. Le nouveau code devrait utiliser les noms commençant par
`Moose_`. Les anciens noms restent disponibles afin de ne pas interrompre les
scripts existants.

| Fonction recommandée | Ancien nom compatible | Rôle |
| --- | --- | --- |
| `Moose_fix_colname()` | `BD_fix_colname()` | Nettoyer les noms de colonnes en remplaçant les espaces et en supprimant certains caractères spéciaux. |
| `Moose_get_duplicates()` | `BD_get_duplicates()` | Trouver les lignes en double à partir d'une ou de plusieurs colonnes clés. |
| `Moose_check_unique()` | `BD_check_unique()` | Vérifier si une colonne contient des valeurs uniques. |
| `Moose_count_unique_categories()` | `BD_count_unique_categories()` | Compter les catégories uniques d'une variable. |
| `Moose_quote_rm()` | `BD_quote_rm()` | Supprimer les guillemets courants des colonnes de type caractère ou facteur. |
| `Moose_1_cat()` | `BD_1_cat()` | Créer un tableau d'effectifs et de pourcentages sur une seule ligne pour une variable catégorielle. |
| `Moose_1_cont()` | `BD_1_cont()` | Créer un tableau récapitulatif sur une seule ligne pour une variable continue. |
| `Moose_compare_balance()` | `BD_compare_balance()` | Comparer l'équilibre des covariables entre deux jeux de données. |
| `Moose_SID_creator()` | `BD_SID_creator()` | Créer un SID stable et désidentifié à partir du nom, de la date de naissance et du genre. |
| `Moose_today()` | `BD_today()` | Retourner la date du jour avec des traits de soulignement et dans un format compact. |
| `Moose_read_csv()` | - | Lire un fichier CSV et nettoyer ses noms de colonnes. |
| `Moose_SRds()` | - | Enregistrer un objet R dans un fichier RDS. |
| `Moose_LRds()` | - | Restaurer un objet R à partir d'un fichier RDS. |
| `Moose_setup_name_masking()` | `setup_name_masking()` | Préparer le moteur spaCy lorsqu'il est disponible, ou la solution de rechange en expressions régulières R. |
| `Moose_check_name_masking()` | `check_name_masking()` | Afficher l'état actuel de la configuration Python, spaCy et du modèle. |
| `Moose_mask_person_names()` | `mask_person_names()` | Remplacer les noms de personnes détectés dans un texte par une chaîne de remplacement. |
| `Moose_detect_person_names()` | `detect_person_names()` | Retourner un tableau d'audit des noms détectés et de leur position dans le texte. |
| `Moose_name_flag()` | - | Retourner un vecteur 1/0 précisant si chaque texte contient un nom de personne détecté. |
| `Moose_apply_name_masking_rules()` | `apply_name_masking_rules()` | Appliquer des règles supplémentaires de masquage des noms avec des expressions régulières. |
| `Moose_todate()` | - | Convertir les formats de date courants en valeurs R `Date`. |
| `Moose_todatetime()` | - | Convertir les formats courants de date et heure en valeurs R `POSIXct`. |
| `Moose_boost_data()` | - | Détecter et convertir les colonnes qui ressemblent à des dates dans un jeu de données. |
| `Moose_load_packages()` | `load_packages()` | Installer les packages manquants et charger une liste de packages. |
| `Moose_load_mooser_packages()` | `load_mooser_packages()` | Charger l'ensemble de packages MooseR par défaut. |
| `Moose_enable_mooser_startup_packages()` | `enable_mooser_startup_packages()` | Activer le chargement automatique des packages au démarrage de R. |
| `Moose_disable_mooser_startup_packages()` | `disable_mooser_startup_packages()` | Désactiver le chargement automatique des packages au démarrage de R. |

### Exemples

Nettoyer les noms de colonnes :

```r
df <- data.frame("Col (1)" = 1:3, "Name's-Age" = c(20, 25, 30))
Moose_fix_colname(df)
```

Résumer une variable catégorielle :

```r
df <- data.frame(group = c("A", "B", "A", NA, "C", "B", "B"))
Moose_1_cat(df, "group", display_name = "Group")
```

Résumer une variable continue :

```r
df <- data.frame(age = c(21, 34, 45, 52, NA, 63))
Moose_1_cont(df, "age", display_name = "Age")
```

Trouver les lignes en double à partir de colonnes clés :

```r
Moose_get_duplicates(mtcars, cyl, gear, .keep_all = FALSE)
```

Vérifier l'unicité :

```r
Moose_check_unique(iris, Species)
Moose_check_unique(iris, "Species", na.rm = TRUE)
```

Créer des SID stables et désidentifiés :

```r
Moose_SID_creator(
  first_name = c("Ann", "Marie"),
  last_name = c("Martin", "Dubois"),
  DOB_ready = c("1990-07-03", "2000/01/02"),
  gender = c("F", "M")
)
```

Comparer l'équilibre entre deux jeux de données :

```r
set.seed(1)
a <- data.frame(x = rnorm(100), g = sample(c("M", "F"), 100, TRUE))
b <- data.frame(x = rnorm(120, 0.2, 1.1), g = sample(c("M", "F"), 120, TRUE))

Moose_compare_balance(a, b, vars = c("x", "g"), include_smd = TRUE)
```

Obtenir la date du jour :

```r
Moose_today()
```

Convertir les formats courants de date et de date et heure :

```r
Moose_todate(c("2024-01-05", "01/06/2024", "20240107"))
Moose_todate(c(20240105, 45296))

Moose_todatetime(c("2024-01-05 13:30:00", "2024/01/06 8:05"))
Moose_todatetime(c(202401051330, 1704450600))
```

Convertir automatiquement les colonnes qui ressemblent à des dates :

```r
raw_data <- data.frame(
  visit_date = c("2024-01-05", "2024/01/06"),
  created_at = c("2024-01-05 13:30", "2024-01-06 08:05"),
  note = c("First visit", "Follow-up")
)

boosted_data <- Moose_boost_data(raw_data)
str(boosted_data)
```

Enregistrer et restaurer un objet R au format RDS :

```r
Moose_SRds(boosted_data, "boosted_data")
restored_data <- Moose_LRds("boosted_data")
```

Préparer le masquage des noms de personnes. Sur un ordinateur standard,
`engine = "auto"` essaie spaCy et utilise les expressions régulières si Python
ne peut pas être initialisé :

```r
Moose_setup_name_masking()
Moose_check_name_masking()
```

Sur un ordinateur de travail verrouillé où Python ne peut pas être modifié,
utilisez directement le moteur entièrement en R :

```r
Moose_setup_name_masking(engine = "regex")
Moose_check_name_masking()
```

En mode regex, `Moose_check_name_masking()` devrait afficher `Engine: regex`.
Python, spaCy et le modèle spaCy ne sont pas utilisés dans ce mode.

Le moteur regex ignore les expressions autonomes entièrement en majuscules,
comme `MEDICAL RECORDS` ou `THE PATIENT`. Il accepte les noms entièrement en
majuscules uniquement après un titre reconnu, comme `RN SMITH`, ou une phrase
de travail, comme `Reviewed by JOHN SMITH`.
Les noms d'organismes de santé se terminant par des mots comme `Hospital`,
`Clinic`, `Centre`, `Health` ou `Foundation` ne sont pas considérés comme des
noms de personnes. Par exemple, `St Mary Hospital` et
`Vancouver General Hospital` sont conservés.
Les expressions cliniques contenant des mots comme `Chest`, `Pain`, `Disease`
ou `Syndrome` sont également conservées ; par exemple, `Cardia Chest Pain`
n'est pas masqué.
Les expressions de traitement comme `Normal Saline IV` sont aussi conservées.
Les libellés de documents cliniques comme `RN Report` sont aussi conservés.
La liste blanche médicale intégrée conserve aussi les expressions avec
majuscules initiales comme `Chief Complaint`, `Present Illness`, `Code Status`,
`On Arrival`, `Interfacility Transfer`, `Transport Truck`, `Ground Level`, `Nursing Home`,
`Altered Mental Status`,
`Nausea Vomiting`, `Nose Bleed`, `Dog Bite`, `Urinary Issues`, `Patient Demographics`,
`Behavioural Problems`, `Substance Abuse`, `Heart Transplant`, `Seizure Activity` et
`Opioid Overdose`.
Cette liste blanche a volontairement priorité sur le modèle de nom avec
majuscules initiales ; vérifiez les données lorsqu'un vrai nom de personne peut
contenir l'un de ces termes.
Les expressions de lieu comme `Private Residence`, `West Side`, `Costco Store`
et `Edmonton International Airport`, ainsi que les états cliniques comme `Pt Syncopal` et
`Heavily Intoxicated`, sont également conservés.
Ces exclusions s'appliquent aux moteurs regex et spaCy.
MooseR conserve également les 334 noms officiels des municipalités de
l'Alberta, notamment `Medicine Hat`, `Red Deer` et `Rocky Mountain House`. Le
registre suit les codes municipaux 2024 du gouvernement de l'Alberta et exclut
l'organisme administratif `Special Areas Board`. Le nom de lieu albertain
`Fort McMurray` et l'orthographe courante `Fort McMurry` sont également
conservés.
MooseR exclut aussi les 1 150 noms uniques du répertoire
[Find Healthcare](https://www.albertahealthservices.ca/findhealth/search.aspx)
d'Alberta Health Services, notamment les hôpitaux, centres de santé, cliniques,
établissements de soins continus et autres lieux de service officiels. Le nom
complet de l'établissement doit figurer dans le texte : `Peter Lougheed Centre`,
`Grand Manor` et `Chartwell Griesbach` sont conservés, tandis qu'un nom de
personne isolé reste détecté.
MooseR exclut également les 2 616 noms uniques d'écoles du
[School Information Report](https://education.alberta.ca/alberta-education/school-authority-index/)
de l'Alberta Education, couvrant les écoles publiques, séparées, francophones,
à charte, indépendantes, des Premières Nations, les opérateurs privés de
services à la petite enfance et les écoles provinciales. La comparaison ne
tient pas compte de la casse et le nom complet de l'école doit figurer dans le
texte. Un nom de personne qui fait partie d'un nom d'école reste détecté lorsque
le nom complet de l'école est absent.

Masquer les noms dans un vecteur de caractères :

```r
comments <- c(
  "John Smith spoke with Sarah Johnson in Vancouver.",
  "The patient was transported to Vancouver.",
  "Reviewed by Michael Brown.",
  "",
  NA
)

Moose_mask_person_names(comments)
```

Forcer l'utilisation du moteur entièrement en R :

```r
Moose_mask_person_names(comments, engine = "regex")
```

Conserver le texte original et le texte masqué côte à côte :

```r
Moose_mask_person_names(comments, keep_original = TRUE)
```

Créer un tableau d'audit des noms détectés :

```r
Moose_detect_person_names(comments)
```

Ajouter un indicateur par ligne pour les noms de personnes détectés :

```r
records <- data.frame(comments = comments)
flagged_records <- dplyr::mutate(
  records,
  name_flag = Moose_name_flag(comments)
)
flagged_records
```

Pour les grands jeux de données, supprimez d'abord les regroupements afin que
la colonne complète soit traitée en un seul appel :

```r
flagged_records <- records |>
  dplyr::ungroup() |>
  dplyr::mutate(
    name_flag = Moose_name_flag(comments, batch_size = 2000L)
  )
```

Appliquer des règles supplémentaires après le masquage spaCy :

```r
masked <- Moose_mask_person_names(comments)
Moose_apply_name_masking_rules(masked)
```

Pour les données médicales réelles ou les données sensibles sur le plan de la
confidentialité, conservez le tableau d'audit et examinez un échantillon du
résultat. La reconnaissance des entités nommées est utile, mais elle ne
garantit pas que tous les noms de personnes seront détectés.

### Licence

Licence MIT. Copyright (c) 2026 LisbonBulldog.

---

<a id="portugues"></a>

## Português

MooseR é um pequeno pacote utilitário de R para tarefas comuns de limpeza de
dados e criação de tabelas de resumo. Inclui funções para limpar nomes de
colunas, detetar duplicados, verificar a unicidade, remover aspas, produzir
resumos numa única linha para variáveis categóricas e contínuas, comparar o
equilíbrio das covariáveis, gerar identificadores SID estáveis e
desidentificados, e converter datas.

As principais funções de limpeza de dados são intencionalmente leves. O
processo de mascaramento de nomes de pessoas pode utilizar `reticulate` e spaCy
quando Python está disponível. Também inclui uma alternativa integralmente em
R, baseada em expressões regulares, para computadores de trabalho bloqueados
onde Python não pode ser alterado. MooseR requer R 4.1.0 ou uma versão
posterior.

### Instalação

Instale a versão de desenvolvimento a partir do GitHub:

```r
install.packages("remotes")
remotes::install_github("MooseWEB3/MooseR", upgrade = "never")
```

Carregue o pacote:

```r
library(MooseR)
```

Para reinstalar a versão mais recente do GitHub sobre uma cópia local
existente:

```r
remotes::install_github("MooseWEB3/MooseR", force = TRUE, upgrade = "never")
```

### Carregamento de pacotes no arranque

Carregue o conjunto predefinido de pacotes do fluxo de trabalho MooseR na
sessão R atual:

```r
Moose_load_mooser_packages()
```

Para ver a lista predefinida:

```r
Moose_default_mooser_packages()
```

Para que R ou RStudio carregue automaticamente estes pacotes sempre que
iniciar, execute este comando uma única vez:

```r
Moose_enable_mooser_startup_packages()
```

Este comando adiciona um bloco gerido pelo MooseR ao ficheiro `~/.Rprofile`.
Reinicie R ou RStudio após a ativação. Para desativar posteriormente o
carregamento automático:

```r
Moose_disable_mooser_startup_packages()
```

### Funções

MooseR mantém os nomes originais das funções para garantir a
retrocompatibilidade. O código novo deve utilizar os nomes com o prefixo
`Moose_`. Os nomes antigos continuam disponíveis para não interromper scripts
existentes.

| Função recomendada | Nome antigo compatível | Finalidade |
| --- | --- | --- |
| `Moose_fix_colname()` | `BD_fix_colname()` | Limpar nomes de colunas, substituindo espaços e removendo determinados caracteres especiais. |
| `Moose_get_duplicates()` | `BD_get_duplicates()` | Encontrar linhas duplicadas através de uma ou mais colunas-chave. |
| `Moose_check_unique()` | `BD_check_unique()` | Verificar se uma coluna contém valores únicos. |
| `Moose_count_unique_categories()` | `BD_count_unique_categories()` | Contar as categorias únicas de uma variável. |
| `Moose_quote_rm()` | `BD_quote_rm()` | Remover caracteres comuns de aspas das colunas de texto e fator. |
| `Moose_1_cat()` | `BD_1_cat()` | Criar uma tabela de contagens e percentagens numa única linha para uma variável categórica. |
| `Moose_1_cont()` | `BD_1_cont()` | Criar uma tabela de resumo numa única linha para uma variável contínua. |
| `Moose_compare_balance()` | `BD_compare_balance()` | Comparar o equilíbrio das covariáveis entre dois conjuntos de dados. |
| `Moose_SID_creator()` | `BD_SID_creator()` | Criar um SID estável e desidentificado a partir do nome, data de nascimento e género. |
| `Moose_today()` | `BD_today()` | Devolver a data atual nos formatos com sublinhados e compacto. |
| `Moose_read_csv()` | - | Ler um ficheiro CSV e limpar os respetivos nomes de colunas. |
| `Moose_SRds()` | - | Guardar um objeto R num ficheiro RDS. |
| `Moose_LRds()` | - | Restaurar um objeto R a partir de um ficheiro RDS. |
| `Moose_setup_name_masking()` | `setup_name_masking()` | Preparar o motor spaCy quando disponível, ou a alternativa em R baseada em expressões regulares. |
| `Moose_check_name_masking()` | `check_name_masking()` | Apresentar o estado atual da configuração de Python, spaCy e do modelo. |
| `Moose_mask_person_names()` | `mask_person_names()` | Substituir os nomes de pessoas detetados num texto por uma cadeia de substituição. |
| `Moose_detect_person_names()` | `detect_person_names()` | Devolver uma tabela de auditoria dos nomes detetados e das respetivas posições no texto. |
| `Moose_name_flag()` | - | Devolver um vetor 1/0 que mostra se cada texto contém um nome de pessoa detetado. |
| `Moose_apply_name_masking_rules()` | `apply_name_masking_rules()` | Aplicar regras adicionais de mascaramento de nomes com expressões regulares. |
| `Moose_todate()` | - | Converter formatos comuns de data em valores R `Date`. |
| `Moose_todatetime()` | - | Converter formatos comuns de data e hora em valores R `POSIXct`. |
| `Moose_boost_data()` | - | Detetar e converter colunas semelhantes a datas num conjunto de dados. |
| `Moose_load_packages()` | `load_packages()` | Instalar os pacotes em falta e carregar uma lista de pacotes. |
| `Moose_load_mooser_packages()` | `load_mooser_packages()` | Carregar o conjunto predefinido de pacotes MooseR. |
| `Moose_enable_mooser_startup_packages()` | `enable_mooser_startup_packages()` | Ativar o carregamento automático de pacotes no arranque do R. |
| `Moose_disable_mooser_startup_packages()` | `disable_mooser_startup_packages()` | Desativar o carregamento automático de pacotes no arranque do R. |

### Exemplos

Limpar nomes de colunas:

```r
df <- data.frame("Col (1)" = 1:3, "Name's-Age" = c(20, 25, 30))
Moose_fix_colname(df)
```

Resumir uma variável categórica:

```r
df <- data.frame(group = c("A", "B", "A", NA, "C", "B", "B"))
Moose_1_cat(df, "group", display_name = "Group")
```

Resumir uma variável contínua:

```r
df <- data.frame(age = c(21, 34, 45, 52, NA, 63))
Moose_1_cont(df, "age", display_name = "Age")
```

Encontrar linhas duplicadas através de colunas-chave:

```r
Moose_get_duplicates(mtcars, cyl, gear, .keep_all = FALSE)
```

Verificar a unicidade:

```r
Moose_check_unique(iris, Species)
Moose_check_unique(iris, "Species", na.rm = TRUE)
```

Criar SID estáveis e desidentificados:

```r
Moose_SID_creator(
  first_name = c("Ann", "Marie"),
  last_name = c("Martin", "Dubois"),
  DOB_ready = c("1990-07-03", "2000/01/02"),
  gender = c("F", "M")
)
```

Comparar o equilíbrio entre dois conjuntos de dados:

```r
set.seed(1)
a <- data.frame(x = rnorm(100), g = sample(c("M", "F"), 100, TRUE))
b <- data.frame(x = rnorm(120, 0.2, 1.1), g = sample(c("M", "F"), 120, TRUE))

Moose_compare_balance(a, b, vars = c("x", "g"), include_smd = TRUE)
```

Obter a data atual:

```r
Moose_today()
```

Converter formatos comuns de data e de data e hora:

```r
Moose_todate(c("2024-01-05", "01/06/2024", "20240107"))
Moose_todate(c(20240105, 45296))

Moose_todatetime(c("2024-01-05 13:30:00", "2024/01/06 8:05"))
Moose_todatetime(c(202401051330, 1704450600))
```

Converter automaticamente colunas semelhantes a datas:

```r
raw_data <- data.frame(
  visit_date = c("2024-01-05", "2024/01/06"),
  created_at = c("2024-01-05 13:30", "2024-01-06 08:05"),
  note = c("First visit", "Follow-up")
)

boosted_data <- Moose_boost_data(raw_data)
str(boosted_data)
```

Guardar e restaurar um objeto R no formato RDS:

```r
Moose_SRds(boosted_data, "boosted_data")
restored_data <- Moose_LRds("boosted_data")
```

Preparar o mascaramento de nomes de pessoas. Num computador normal,
`engine = "auto"` tenta utilizar spaCy e recorre às expressões regulares se
Python não puder ser inicializado:

```r
Moose_setup_name_masking()
Moose_check_name_masking()
```

Num computador de trabalho bloqueado onde Python não pode ser alterado, utilize
diretamente o motor integralmente em R:

```r
Moose_setup_name_masking(engine = "regex")
Moose_check_name_masking()
```

No modo regex, `Moose_check_name_masking()` deve apresentar `Engine: regex`.
Python, spaCy e o modelo spaCy não são utilizados neste modo.

O motor regex ignora expressões isoladas totalmente em maiúsculas, como
`MEDICAL RECORDS` ou `THE PATIENT`. Aceita nomes totalmente em maiúsculas
apenas depois de um título reconhecido, como `RN SMITH`, ou de uma expressão de
trabalho, como `Reviewed by JOHN SMITH`.
Os nomes de organizações de saúde terminados em palavras como `Hospital`,
`Clinic`, `Centre`, `Health` ou `Foundation` não são tratados como nomes de
pessoas. Por exemplo, `St Mary Hospital` e `Vancouver General Hospital` são
preservados.
As expressões clínicas que contêm palavras como `Chest`, `Pain`, `Disease` ou
`Syndrome` também são preservadas; por exemplo, `Cardia Chest Pain` não é
mascarado.
As expressões de tratamento como `Normal Saline IV` também são preservadas.
Os rótulos de documentos clínicos como `RN Report` também são preservados.
A lista médica integrada também preserva expressões iniciadas por maiúsculas,
como `Chief Complaint`, `Present Illness`, `Code Status`, `On Arrival`,
`Interfacility Transfer`, `Transport Truck`, `Ground Level`, `Nursing Home`, `Altered Mental Status`,
`Nausea Vomiting`, `Nose Bleed`, `Dog Bite`, `Urinary Issues`, `Patient Demographics`,
`Behavioural Problems`, `Substance Abuse`,
`Heart Transplant`, `Seizure Activity` e `Opioid Overdose`.
Esta lista tem prioridade intencional sobre o padrão de nome com iniciais
maiúsculas; audite os dados quando um nome real puder conter um destes termos.
As expressões de local como `Private Residence`, `West Side`, `Costco Store` e
`Edmonton International Airport`,
bem como os estados clínicos como `Pt Syncopal` e `Heavily Intoxicated`, também
são preservados.
Estas exclusões aplicam-se aos motores regex e spaCy.
O MooseR também preserva os 334 nomes oficiais dos municípios de Alberta,
incluindo `Medicine Hat`, `Red Deer` e `Rocky Mountain House`. O registo segue
os Códigos Municipais de 2024 do Governo de Alberta e exclui o órgão
administrativo `Special Areas Board`. O nome do local de Alberta
`Fort McMurray` e a grafia comum `Fort McMurry` também são preservados.
O MooseR também exclui os 1.150 nomes únicos do diretório
[Find Healthcare](https://www.albertahealthservices.ca/findhealth/search.aspx)
da Alberta Health Services, incluindo hospitais, centros de saúde, clínicas,
unidades de cuidados continuados e outros locais oficiais de serviço. O nome
completo da instituição deve aparecer no texto: `Peter Lougheed Centre`,
`Grand Manor` e `Chartwell Griesbach` são preservados, enquanto um nome pessoal
isolado continua a ser detetado.
O MooseR também exclui os 2.616 nomes únicos de escolas do
[School Information Report](https://education.alberta.ca/alberta-education/school-authority-index/)
da Alberta Education, abrangendo escolas públicas, separadas, francófonas,
charter, independentes, das Primeiras Nações, operadores privados de educação
infantil e escolas provinciais. A correspondência não diferencia maiúsculas de
minúsculas e o nome completo da escola deve aparecer no texto. Um nome pessoal
que também faça parte do nome de uma escola continua a ser detetado quando o
nome completo da escola não está presente.

Mascarar nomes num vetor de texto:

```r
comments <- c(
  "John Smith spoke with Sarah Johnson in Vancouver.",
  "The patient was transported to Vancouver.",
  "Reviewed by Michael Brown.",
  "",
  NA
)

Moose_mask_person_names(comments)
```

Forçar a utilização do motor integralmente em R:

```r
Moose_mask_person_names(comments, engine = "regex")
```

Manter o texto original e o texto mascarado lado a lado:

```r
Moose_mask_person_names(comments, keep_original = TRUE)
```

Criar uma tabela de auditoria dos nomes detetados:

```r
Moose_detect_person_names(comments)
```

Adicionar um indicador por linha para nomes de pessoas detetados:

```r
records <- data.frame(comments = comments)
flagged_records <- dplyr::mutate(
  records,
  name_flag = Moose_name_flag(comments)
)
flagged_records
```

Para conjuntos de dados grandes, remova primeiro os agrupamentos para que a
coluna completa seja processada numa única chamada:

```r
flagged_records <- records |>
  dplyr::ungroup() |>
  dplyr::mutate(
    name_flag = Moose_name_flag(comments, batch_size = 2000L)
  )
```

Aplicar regras adicionais depois do mascaramento com spaCy:

```r
masked <- Moose_mask_person_names(comments)
Moose_apply_name_masking_rules(masked)
```

Para dados médicos reais ou dados sensíveis em termos de privacidade, mantenha
a tabela de auditoria e reveja uma amostra do resultado. O reconhecimento de
entidades nomeadas é útil, mas não garante que todos os nomes de pessoas sejam
detetados.

### Licença

Licença MIT. Copyright (c) 2026 LisbonBulldog.
