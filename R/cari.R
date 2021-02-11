#' Cari dataset
#'
#' Fungsi untuk mencari dataset yang tersedia di Open Data Kota Jakarta. Saat ini URL dataset yang ditampilkan hanya yang berasal dari peladen data.jakarta.go.id.
#'
#' @param kata_kunci kata kunci dataset yang ingin dicari, case insensitive
#'
#' @return dataframe dengan 5 kolom, yaitu:
#' \describe{
#'   \item{nama}{Judul dataset}
#'   \item{deskripsi}{Penjelasan mengenai dataset}
#'   \item{url}{url lokasi dataset di peladen}
#'   \item{dibuat}{Tanggal dataset dibuat atau diunggah, dalam format YYYY-MM-DD}
#'   \item{diperbaharui}{Tanggal dataset terakhir diperbaharui, dalam format YYYY-MM-DD}
#' }
#'
#' @source Dataset bersumber dari Open Data Kota Jakarta \url{http://data.jakarta.go.id}.
#'
#' @import dplyr stringr
#'
#' @export
#'
#' @examples
#'
#' library(jakartaraya)
#'
#' kebakaran <- cari(kata_kunci = "kebakaran")
#' kebakaran
#'
#' @export


cari <- function(kata_kunci) {

  if (missing(kata_kunci)) {
    stop("kata_kunci belum dimasukan!", call. = FALSE)
  }

  if (any(nchar(kata_kunci) < 2)) {
    stop("kata_kunci harus lebih dari satu huruf!", call. = FALSE)
  }

  kueri <- daftar_dataset %>%
    pull(nama) %>%
    str_to_lower() %>%
    str_subset(pattern = str_to_lower(kata_kunci))

  res <-
    daftar_dataset %>%
    mutate(
      nama_ = str_to_lower(nama)
    ) %>%
    filter(
      nama_ %in% kueri
    ) %>%
    select(-nama_)

  class(res) <- append(class(res), "jktry")

  if (nrow(res) == 0) {
    res <- NULL
    warning(
      "Tidak berhasil menemukan dataset dengan dengan kata kunci: ",
      kata_kunci,
      ". Silakan coba kata kunci lainnnya!",
      call. = FALSE
    )
    return(res)
  } else {
    message(
      "Berhasil menemukan ",
      nrow(res),
      " dataset dengan dengan kata kunci: ",
      kata_kunci
    )
    return(res)
  }
}
