# Documentation for all built-in datasets in the XploreQuran package
# These datasets are pre-downloaded from tanzil.net and bundled for offline use.

#' Quran Arabic Text
#'
#' @description
#' The complete Arabic text of the Quran with metadata for all 6236 ayahs,
#' sourced from the \pkg{quRan} package.
#'
#' @format A data frame with 6236 rows and 18 columns:
#' \describe{
#'   \item{surah_id}{Integer. Surah number (1–114).}
#'   \item{ayah_id}{Integer. Global ayah number (1–6236).}
#'   \item{surah_title_ar}{Character. Surah name in Arabic.}
#'   \item{surah_title_en}{Character. Surah name in English transliteration.}
#'   \item{surah_title_en_trans}{Character. Surah name translated to English.}
#'   \item{revelation_type}{Character. \code{"Meccan"} or \code{"Medinan"}.}
#'   \item{text}{Character. Arabic ayah text.}
#'   \item{surah}{Integer. Surah number (duplicate of surah_id, from quRan source).}
#'   \item{ayah}{Integer. Ayah number within surah.}
#'   \item{ayah_title}{Character. Ayah reference label (e.g. \code{"1:1"}).}
#'   \item{juz}{Integer. Juz number (1–30).}
#'   \item{manzil}{Integer. Manzil number (1–7).}
#'   \item{page}{Integer. Mushaf page number.}
#'   \item{hizb_quarter}{Integer. Hizb quarter number.}
#'   \item{sajda}{Logical. Whether this ayah requires prostration (sajda).}
#'   \item{sajda_id}{Integer. Sajda sequence number, \code{NA} if not a sajda ayah.}
#'   \item{sajda_recommended}{Logical. Whether sajda is recommended, \code{NA} if not applicable.}
#'   \item{sajda_obligatory}{Logical. Whether sajda is obligatory, \code{NA} if not applicable.}
#' }
#' @source \url{https://github.com/andrewheiss/quRan}
"quran_arab"


#' Quran Surah Index
#'
#' @description
#' A reference table of all 114 Surahs with their names and metadata.
#'
#' @format A data frame with 114 rows and columns including surah number,
#'   Arabic name, English transliteration, English translation, revelation type,
#'   and number of ayahs.
#' @source \url{https://github.com/andrewheiss/quRan}
"quran_index"


#' Quran Translation: Saheeh International (English)
#'
#' @description
#' English translation of the Quran by Saheeh International, downloaded from
#' \url{https://tanzil.net/trans/en.sahih}. A \code{translationList} object
#' produced by \code{\link{tanzil_translation}}.
#'
#' @format A \code{translationList} (S3) object — a named list with:
#' \describe{
#'   \item{translation_text}{Data frame with 6236 rows. Key columns:
#'     \code{ayah_id}, \code{surah_id}, \code{surah_ayah_id}, \code{juz},
#'     \code{surah_title_en}, \code{revelation_type}, \code{translation}.}
#'   \item{translation_info}{Data frame with translator metadata.}
#' }
#' @source \url{https://tanzil.net/trans/en.sahih}
"trans_en_sahih"


#' Quran Translation: Abdullah Yusuf Ali (English)
#'
#' @description
#' English translation of the Quran by Abdullah Yusuf Ali, downloaded from
#' \url{https://tanzil.net/trans/en.yusufali}. A \code{translationList} object
#' produced by \code{\link{tanzil_translation}}.
#'
#' @format A \code{translationList} (S3) object. See \code{\link{trans_en_sahih}}
#'   for full format description.
#' @source \url{https://tanzil.net/trans/en.yusufali}
"trans_en_yusufali"


#' Quran Translation: Mohammed Marmaduke Pickthall (English)
#'
#' @description
#' English translation of the Quran by Mohammed Marmaduke Pickthall, downloaded
#' from \url{https://tanzil.net/trans/en.pickthall}. A \code{translationList}
#' object produced by \code{\link{tanzil_translation}}.
#'
#' @format A \code{translationList} (S3) object. See \code{\link{trans_en_sahih}}
#'   for full format description.
#' @source \url{https://tanzil.net/trans/en.pickthall}
"trans_en_pickthall"


#' Quran Translation: Abdullah Muhammad Basmeih (Malay)
#'
#' @description
#' Malay translation of the Quran by Abdullah Muhammad Basmeih, downloaded from
#' \url{https://tanzil.net/trans/ms.basmeih}. A \code{translationList} object
#' produced by \code{\link{tanzil_translation}}.
#'
#' @format A \code{translationList} (S3) object. See \code{\link{trans_en_sahih}}
#'   for full format description.
#' @source \url{https://tanzil.net/trans/ms.basmeih}
"trans_ms_basmeih"


#' Quran Translation: Indonesian Ministry of Religious Affairs (Indonesian)
#'
#' @description
#' Indonesian translation of the Quran by the Indonesian Ministry of Religious
#' Affairs, downloaded from \url{https://tanzil.net/trans/id.indonesian}. A
#' \code{translationList} object produced by \code{\link{tanzil_translation}}.
#'
#' @format A \code{translationList} (S3) object. See \code{\link{trans_en_sahih}}
#'   for full format description.
#' @source \url{https://tanzil.net/trans/id.indonesian}
"trans_id_indonesian"
