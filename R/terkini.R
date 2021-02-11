#' Cari dataset terkini
#'
#' Fungsi untuk mencari dataset terkini yang tersedia di Open Data Kota Jakarta. Saat ini URL dataset yang ditampilkan hanya yang berasal dari peladen data.jakarta.go.id.
#'
#' @param n jumlah dataset yang diinginkan
#' @param berdasarkan dataset terkini berdasarkan tanggal dibuat atau tanggal diperbaharui? Opsinya adalah 'dibuat' dan 'diperbaharui'
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
#' @import dplyr
#'
#' @export
#'
#' @examples
#'
#' library(jakartaraya)
#'
#' dataset_terkini <- terkini(n = 10)
#' dataset_terkini
#'
#' @export

terkini <- function(n = 5, berdasarkan = c("dibuat", "diperbaharui")) {
  if (n < 1 ) {
    stop("n harus lebih dari atau sama dengan 1!", call. = FALSE)
  }

  if (!any(berdasarkan %in% c("dibuat", "diperbaharui"))) {
    stop("Data terkini harus berdasarkan tanggal 'dibuat' atau tanggal 'diperbaharui'!", call. = FALSE)
  }

  res <- daftar_dataset %>%
    arrange(
      desc(
        switch(berdasarkan[[1]],
                dibuat = dibuat,
                diperbaharui = diperbaharui
        )
      )
    ) %>%
    slice(seq_len(n))
  class(res) <- append(class(res), "jktry")
  return(res)
}
