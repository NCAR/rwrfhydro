#' @export
xr_datatable <- function(file) {
  library(data.table)
  library(reticulate)
  xr = import('xarray', convert=FALSE)
  pathlib = import('pathlib', convert=TRUE)
  py = import_builtins(convert=TRUE)
  dataset = xr$open_dataset(file)
  data_array_to_table <- function(var_name = NULL, timedelta_res = 'h') {
    if(is.null(var_name)) {print(dataset) ; return(invisible(NULL))}
    array = dataset[var_name]
    if(!py$isinstance(array, xr$DataArray)){
      stop('The passed array is not an xarray.DataArray.')}
    # array = gof_gage_ds$value
    array_values = py_to_r(array$values)
    array_dims = unlist(py_to_r(array$dims))
    time_res = NULL
    if(timedelta_res == 'h') time_res = 60*(60*1e9)
    if(is.null(time_res)) stop('An appropriate timedelta_res is not available')
    for (ii in 1:length(array_dims)) {
      dim_values = array[[array_dims[ii]]]
      if(py_to_r(dim_values$dtype) == 'timedelta64[ns]'){
        dim_values = py_to_r(dim_values$values$astype('int')) / time_res  # ns
      } else dim_values = py_to_r(dim_values$values)
      dimnames(array_values)[[ii]] = as.list(dim_values)
      names(dimnames(array_values))[ii] = array_dims[ii]
    }
    return(invisible(as.data.table(reshape2::melt(array_values, value.name=array$name))))
  }
}
