AC_INIT(src/Makevars.in)

# Initialize header configuration for pre-processing directives
AC_CONFIG_HEADERS([src/regrid_header.h])

echo "configure.ac: starting"

: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
        echo "Could not determine R_HOME"
        exit 1
fi

#---------------------------------------------------
# Configuration for building rwrfhydro regridding
# dependecies (ESMF, ECMWF GRIB-API). The R
# package rwrfhydro will continue to build
# in absence of the required packages. 
# 
# Logan Karsten
# National Center for Atmospheric Research
# Research Applications Laboratory
# December 17th, 2015
# 
#---------------------------------------------------
FC=`${R_HOME}/bin/R CMD config FC`
FFLAGS=`${R_HOME}/bin/R CMD config FCFLAGS`

REGRID_FLAG_CONFIG=0

# rwrfhydro regridding requires the ECMWF GRIB API.
AC_PATH_GRIB_API
if test $HAS_GRIB_API = "no"; then
	echo "---------------------------------------------------"
	echo "rwrfhydro regridding requires the ECMWF GRIB API"
	echo "to be installed on your system. Please visit"
	echo "https://software.ecmwf.int/wiki/display/GRIB/Home"
	echo "to install. rwrfhydro will install without"
	echo "regridding support."
	echo "---------------------------------------------------"
fi

# rwrfhydro regridding requires the ESMF library.
AC_PATH_ESMF
if test $HAS_ESMF = "no"; then
	echo "---------------------------------------------------"
	echo "rwrfhydro regridding requires the ESMF library"
	echo "to be installed on your system. Please visit"
	echo "https://www.earthsystemcog.org/projects/esmf/"
	echo "to install. rwrfhydro will install without"
	echo "regridding support."
	echo "---------------------------------------------------"
fi

if test $HAS_GRIB_API = "yes"; then
	if test $HAS_ESMF = "yes"; then
		AC_DEFINE([REGRID_FLAG],1)
	fi
fi

REGRID_PKG_LIBS="${GRIB_API_LDFLAGS} ${ESMF_LDFLAGS}"
REGRID_PKG_FCFLAGS="${GRIB_API_FCFLAGS} ${ESMF_FCFLAGS} -cpp -H"
REGRID_PKG_FFLAGS="${GRIB_API_FFLAGS} ${ESMF_FFLAGS} -cpp -H"

AC_SUBST(REGRID_PKG_LIBS)
AC_SUBST(REGRID_PKG_FCFLAGS)
AC_SUBST(REGRID_PKG_FFLAGS)

AC_OUTPUT(src/Makevars)

echo " "
echo "************************ Results of rwrfhydro regridding configure ******************************"
echo " "
echo "GRIB API LD flags       = $GRIB_API_LDFLAGS"
echo "GRIB API FC flags       = $GRIB_API_FCFLAGS"
echo "GRIB API FF flags       = $GRIB_API_FFLAGS"
echo "ESMF LD flags           = $ESMF_LDFLAGS"
echo "ESMF FC flags           = $ESMF_FCFLAGS"
echo "ESMF FF flags           = $ESMF_FFLAGS"
echo " "
echo "R Fortran Compiler used = $FC $FCFLAGS"
echo " "
echo "*************************************************************************************************"
