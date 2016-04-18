dnl
dnl AC_PATH_ESMF()
dnl
dnl This macro looks for the ESMF library.
dnl
dnl Defines the ESMF_LDFLAGS, ESMF_FCFLAGS, and 
dnl ESMF_FFLAGS which are required for Rwrfhydro regridding.
dnl
dnl version 1.0
dnl 18 Jan 2016
dnl Logan Karsten
dnl National Center for Atmospheric Research
dnl Research Applications Laboratory
dnl karsten@ucar.edu
dnl
dnl *******************************************************************************
dnl This code is in the public domain, and can be used for any purposes whatsoever.
dnl *******************************************************************************
dnl
AC_DEFUN([AC_PATH_ESMF],[
AC_ARG_WITH( ESMF_Info, [ --with-ESMF_Info=path to ESMF_Info binary from ESMF package], ESMF_INFO=$withval)
dnl
dnl ======================================
dnl parse path to ESMF_info and compose mod/lib directories.
err=0
if test x$ESMF_INFO = x; then
	ESMF_INFO=`which ESMF_Info`
	ESMF_SHORT=ESMF_Info
	ESMF_INFO_PATH=$PATH
else
	echo "user specified ESMF_Info is $ESMF_INFO"
	ESMF_SHORT=`basename $ESMF_INFO`
	ESMF_INFO_PATH=`dirname $ESMF_INFO`
fi

dnl check to make sure program exists. If it doesn't, pass back appropriate flags
AC_CHECK_PROG( HAS_ESMF, [$ESMF_SHORT], [yes], [no], [$ESMF_INFO_PATH] )
if test x$HAS_ESMF = xno; then
	echo "==========================================="
	echo " ESMF binary check failed indicating "
	echo " ESMF was not properly installed. Please "
	echo " rebuild ESMF if you desire regridding "
	echo " support for Rwrfhydro. Otherwise, the "
	echo " package will continue to build without "
	echo " regridding support. "
	echo "=========================================="
	ESMF_LDFLAGS=""
	ESMF_FCFLAGS=""
	ESMF_FFLAGS=""
else
dnl     parse path
	ESMF_PARSE=$(echo $ESMF_INFO | tr "/" "\n")
	for SEG in $ESMF_PARSE; do
		if  test x$SEG == xbin ; then
			ESMF_LIB_DIR="$ESMF_LIB_DIR/lib"
			ESMF_MOD_DIR="$ESMF_MOD_DIR/mod"
		elif  test x$SEG == xbinO ; then
			ESMF_LIB_DIR="$ESMF_LIB_DIR/libO"
			ESMF_MOD_DIR="$ESMF_MOD_DIR/modO"
		elif test x$SEG == xESMF_Info ; then
			break
		else
			ESMF_LIB_DIR="$ESMF_LIB_DIR/$SEG"
			ESMF_MOD_DIR="$ESMF_MOD_DIR/$SEG"
		fi	
	done
	ESMF_LDFLAGS="-L${ESMF_LIB_DIR} -lesmf"
	ESMF_FCFLAGS="-I${ESMF_MOD_DIR}"
	ESMF_FFLAGS="-I${ESMF_MOD_DIR}"
fi
dnl export variables
AC_SUBST(ESMF_LDFLAGS)
AC_SUBST(ESMF_FCFLAGS)
AC_SUBST(ESMF_FFLAGS)
])

