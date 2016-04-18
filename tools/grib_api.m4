dnl
dnl AC_PATH_GRIB_API()
dnl
dnl This macro looks for the ECMWF GRIB API used to read GRIB
dnl data in. 
dnl
dnl Defines GRIB_API_LDFLAGS, GRIB_API_FCFLAGS, and 
dnl GRIB_API_FFLAGS
dnl
dnl version 1.0
dnl 18 Jan 2016
dnl Logan R. Karsten
dnl National Center for Atmospheric Research
dnl Research Applications Laboratory
dnl karsten@ucar.edu
dnl
dnl *******************************************************************************
dnl This code is in the public domain, and can be used for any purposes whatsoever.
dnl *******************************************************************************
dnl
AC_DEFUN([AC_PATH_GRIB_API],[
AC_ARG_WITH( grib_dump, [ --with-grib_dump=path to GRIB dump binary from API package], GRIB_DUMP=$withval)
dnl
dnl
dnl ======================================
dnl parse path to ESMF_info and compose mod/lib directories.
err=0
if test x$GRIB_DUMP = x; then
	GRIB_DUMP=`which grib_dump`
	GRIB_DUMP_SHORT=grib_dump
	GRIB_DUMP_PATH=$PATH
else
	echo "user specified grib_dump is $GRIB_DUMP"
	GRIB_DUMP_SHORT=`basename $GRIB_DUMP`
	GRIB_DUMP_PATH=`dirname $GRIB_DUMP`
fi

dnl check to make sure program exists. If it doesn't, pass back appropriate flags.
AC_CHECK_PROG( HAS_GRIB_API, [$GRIB_DUMP_SHORT], [yes], [no], [$GRIB_DUMP_PATH] )
if test x$HAS_GRIB_API = xno; then
	echo "==========================================="
	echo " ECMWF GRIB API binary check failed "
	echo " indicating the API was not properly "
	echo " installed. Please rebuild the API if you "
	echo " desire regridding support for Rwrfhydro. "
	echo " Otherwise, the package will continue to "
	echo " build without regridding support."
	echo "==========================================="
	GRIB_API_LDFLAGS=""
	GRIB_API_FCFLAGS=""
	GRIB_API_FFLAGS=""
else
dnl     parse path
	GRIB_API_PARSE=$(echo $GRIB_DUMP | tr "/" "\n")
	for SEG in $GRIB_API_PARSE; do
		if test x$SEG == xbin ; then
			GRIB_API_LIB_DIR="$GRIB_API_LIB_DIR/lib"
			GRIB_API_INC_DIR="$GRIB_API_INC_DIR/include"
		elif test x$SEG == xgrib_dump ; then
			break
		else
			GRIB_API_LIB_DIR="$GRIB_API_LIB_DIR/$SEG"
			GRIB_API_INC_DIR="$GRIB_API_INC_DIR/$SEG"
		fi
	done
	GRIB_API_LDFLAGS="-L${GRIB_API_LIB_DIR} -lgrib_api_f90 -lgrib_api"
	GRIB_API_FCFLAGS="-I${GRIB_API_INC_DIR}"
	GRIB_API_FFLAGS="-I${GRIB_API_INC_DIR}"
fi
dnl export variables
AC_SUBST(GRIB_API_LDFLAGS)
AC_SUBST(GRIB_API_FCFLAGS)
AC_SUBST(GRIB_API_FFLAGS)
])
