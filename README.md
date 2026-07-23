# MooseR

[English](#english) | [Français](#francais)

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
| `Moose_setup_name_masking()` | `setup_name_masking()` | Prepare the spaCy engine when available, or the pure R regex fallback. |
| `Moose_check_name_masking()` | `check_name_masking()` | Report the current Python, spaCy, and model setup status. |
| `Moose_mask_person_names()` | `mask_person_names()` | Replace detected personal names in text with a replacement string. |
| `Moose_detect_person_names()` | `detect_person_names()` | Return an audit table of detected names and character offsets. |
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
| `Moose_setup_name_masking()` | `setup_name_masking()` | Préparer le moteur spaCy lorsqu'il est disponible, ou la solution de rechange en expressions régulières R. |
| `Moose_check_name_masking()` | `check_name_masking()` | Afficher l'état actuel de la configuration Python, spaCy et du modèle. |
| `Moose_mask_person_names()` | `mask_person_names()` | Remplacer les noms de personnes détectés dans un texte par une chaîne de remplacement. |
| `Moose_detect_person_names()` | `detect_person_names()` | Retourner un tableau d'audit des noms détectés et de leur position dans le texte. |
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
