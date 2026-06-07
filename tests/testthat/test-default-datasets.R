library(testthat)
library(XploreQuran)

# =========================================================================
# Expected canonical structure produced by the current tanzil_translation()
# =========================================================================

# The canonical (minimum required) columns that MUST be present in every
# translation_text data frame, as required by preprocess_tokens() and
# all analytic functions.
REQUIRED_COLUMNS <- c(
  "ayah_id",
  "surah_id",
  "surah_ayah_id",
  "juz",
  "surah_title_ar",
  "surah_title_en",
  "surah_title_en_trans",
  "revelation_type",
  "translation"
)

REQUIRED_COL_TYPES <- list(
  ayah_id             = "integer",
  surah_id            = "integer",
  surah_ayah_id       = "integer",
  juz                 = c("integer", "numeric"),  # accept both
  translation         = "character",
  surah_title_en      = "character",
  revelation_type     = "character"
)

EXPECTED_NROW         <- 6236L
EXPECTED_SURAH_RANGE  <- c(1L, 114L)
EXPECTED_JUZ_RANGE    <- c(1L,  30L)
EXPECTED_AYAH_RANGE   <- c(1L, 6236L)

# All five default datasets bundled in the package
DATASET_NAMES <- c(
  "trans_en_sahih",
  "trans_en_yusufali",
  "trans_en_pickthall",
  "trans_ms_basmeih",
  "trans_id_indonesian"
)

# =========================================================================
# Helper: load a named package dataset
# =========================================================================
load_pkg_dataset <- function(name) {
  e <- new.env(parent = emptyenv())
  data(list = name, package = "XploreQuran", envir = e)
  get(name, envir = e)
}

# =========================================================================
# Test Suite: Default Dataset Integrity
# =========================================================================

for (ds_name in DATASET_NAMES) {

  # ---- 1. S3 Class -------------------------------------------------------
  test_that(sprintf("[%s] has class 'translationList'", ds_name), {
    ds <- load_pkg_dataset(ds_name)
    expect_s3_class(ds, "translationList")
  })

  # ---- 2. List structure -------------------------------------------------
  test_that(sprintf("[%s] is a list with 'translation_text' and 'translation_info'", ds_name), {
    ds <- load_pkg_dataset(ds_name)
    expect_type(ds, "list")
    expect_true("translation_text" %in% names(ds),
                label = "'translation_text' element present")
    expect_true("translation_info" %in% names(ds),
                label = "'translation_info' element present")
  })

  # ---- 3. trans_indicator attribute --------------------------------------
  test_that(sprintf("[%s] has non-empty 'trans_indicator' attribute", ds_name), {
    ds <- load_pkg_dataset(ds_name)
    ti <- attr(ds, "trans_indicator")
    expect_false(is.null(ti),       label = "trans_indicator is not NULL")
    expect_true(nchar(ti) > 0,     label = "trans_indicator is non-empty")
  })

  # ---- 4. Exact row count ------------------------------------------------
  test_that(sprintf("[%s] translation_text has exactly 6236 rows", ds_name), {
    ds <- load_pkg_dataset(ds_name)
    expect_equal(nrow(ds$translation_text), EXPECTED_NROW)
  })

  # ---- 5. Required columns present ---------------------------------------
  test_that(sprintf("[%s] translation_text contains all required columns", ds_name), {
    ds  <- load_pkg_dataset(ds_name)
    missing_cols <- setdiff(REQUIRED_COLUMNS, names(ds$translation_text))
    expect_equal(
      length(missing_cols), 0L,
      label = paste("Missing columns:", paste(missing_cols, collapse = ", "))
    )
  })

  # ---- 6. Column types ---------------------------------------------------
  test_that(sprintf("[%s] required columns have correct types", ds_name), {
    ds  <- load_pkg_dataset(ds_name)
    txt <- ds$translation_text
    for (col in names(REQUIRED_COL_TYPES)) {
      if (col %in% names(txt)) {
        expect_true(
          class(txt[[col]]) %in% REQUIRED_COL_TYPES[[col]],
          label = sprintf("Column '%s' type is '%s' (expected: %s)",
                          col, class(txt[[col]]),
                          paste(REQUIRED_COL_TYPES[[col]], collapse = " or "))
        )
      }
    }
  })

  # ---- 7. No NA in key ID columns ----------------------------------------
  test_that(sprintf("[%s] key ID columns have no NA values", ds_name), {
    ds  <- load_pkg_dataset(ds_name)
    txt <- ds$translation_text
    for (col in c("ayah_id", "surah_id", "surah_ayah_id")) {
      if (col %in% names(txt)) {
        expect_equal(
          sum(is.na(txt[[col]])), 0L,
          label = sprintf("Column '%s' has no NAs", col)
        )
      }
    }
  })

  # ---- 8. No NA in translation column ------------------------------------
  test_that(sprintf("[%s] 'translation' column has no NA values", ds_name), {
    ds  <- load_pkg_dataset(ds_name)
    txt <- ds$translation_text
    if ("translation" %in% names(txt)) {
      expect_equal(sum(is.na(txt$translation)), 0L)
    } else {
      fail("'translation' column is missing")
    }
  })

  # ---- 9. surah_id range -------------------------------------------------
  test_that(sprintf("[%s] surah_id values within 1–114", ds_name), {
    ds  <- load_pkg_dataset(ds_name)
    txt <- ds$translation_text
    if ("surah_id" %in% names(txt)) {
      expect_true(all(txt$surah_id >= EXPECTED_SURAH_RANGE[1] &
                        txt$surah_id <= EXPECTED_SURAH_RANGE[2]))
    }
  })

  # ---- 10. juz column present AND range 1–30 ----------------------------
  test_that(sprintf("[%s] 'juz' column present and values within 1–30", ds_name), {
    ds  <- load_pkg_dataset(ds_name)
    txt <- ds$translation_text
    expect_true("juz" %in% names(txt),
                label = "'juz' column must be present for analytic filtering")
    if ("juz" %in% names(txt)) {
      expect_true(all(!is.na(txt$juz)),
                  label = "'juz' column has no NA values")
      expect_true(all(txt$juz >= EXPECTED_JUZ_RANGE[1] &
                        txt$juz <= EXPECTED_JUZ_RANGE[2]),
                  label = "All juz values within 1–30")
    }
  })

  # ---- 11. ayah_id global range 1–6236 ----------------------------------
  test_that(sprintf("[%s] ayah_id (global) values within 1–6236", ds_name), {
    ds  <- load_pkg_dataset(ds_name)
    txt <- ds$translation_text
    if ("ayah_id" %in% names(txt)) {
      expect_true(all(txt$ayah_id >= EXPECTED_AYAH_RANGE[1] &
                        txt$ayah_id <= EXPECTED_AYAH_RANGE[2]))
    }
  })

  # ---- 12. translation_info structure ------------------------------------
  test_that(sprintf("[%s] translation_info has 'info' and 'value' columns", ds_name), {
    ds <- load_pkg_dataset(ds_name)
    expect_true("info"  %in% names(ds$translation_info))
    expect_true("value" %in% names(ds$translation_info))
  })

  # ---- 13. Usable by trans_analytic_config + preprocess_tokens ----------
  test_that(sprintf("[%s] is usable by tf_trans() with default config", ds_name), {
    ds  <- load_pkg_dataset(ds_name)
    cfg <- trans_analytic_config(
      by               = "surah",
      sub_by           = 1L,          # Al-Fatihah only for speed
      remove_stopwords = FALSE,
      normalize        = FALSE
    )
    result <- tf_trans(ds, config = cfg)
    expect_s3_class(result, "data.frame")
    expect_gt(nrow(result), 0L)
    expect_true(all(c("word", "n", "tf", "tf_idf") %in% names(result)))
  })
}
