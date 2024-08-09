﻿using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        ///    DSYSVXX uses the diagonal pivoting factorization to compute the
        ///    solution to a double precision system of linear equations A * X = B, where A
        ///    is an N-by-N symmetric matrix and X and B are N-by-NRHS matrices.
        /// </para>
        /// <para>
        ///    If requested, both normwise and maximum componentwise error bounds
        ///    are returned. DSYSVXX will return a solution with a tiny
        ///    guaranteed error (O(eps) where eps is the working machine
        ///    precision) unless the matrix is very ill-conditioned, in which
        ///    case a warning is returned. Relevant condition numbers also are
        ///    calculated and returned.
        /// </para>
        /// <para>
        ///    DSYSVXX accepts user-provided factorizations and equilibration
        ///    factors; see the definitions of the FACT and EQUED options.
        ///    Solving with refinement and using a factorization from a previous
        ///    DSYSVXX call will also produce a solution with either O(eps)
        ///    errors or warnings, but we cannot make that claim for general
        ///    user-provided factorizations and equilibration factors if they
        ///    differ from what DSYSVXX would itself produce.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="fact">
        /// [in] FACT is CHARACTER*1.
        /// Specifies whether or not the factored form of the matrix A is
        /// supplied on entry, and if not, whether the matrix A should be
        /// equilibrated before it is factored.
        /// = &#39;F&#39;:  On entry, AF and IPIV contain the factored form of A.
        /// If EQUED is not &#39;N&#39;, the matrix A has been
        /// equilibrated with scaling factors given by S.
        /// A, AF, and IPIV are not modified.
        /// = &#39;N&#39;:  The matrix A will be copied to AF and factored.
        /// = &#39;E&#39;:  The matrix A will be equilibrated if necessary, then
        /// copied to AF and factored.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// = &#39;U&#39;:  Upper triangle of A is stored;
        /// = &#39;L&#39;:  Lower triangle of A is stored.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of linear equations, i.e., the order of the
        /// matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="nrhs">
        /// [in] NRHS is INTEGER.
        /// The number of right hand sides, i.e., the number of columns
        /// of the matrices B and X.  NRHS &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is DOUBLE PRECISION array, dimension (LDA,N).
        /// The symmetric matrix A.  If UPLO = &#39;U&#39;, the leading N-by-N
        /// upper triangular part of A contains the upper triangular
        /// part of the matrix A, and the strictly lower triangular
        /// part of A is not referenced.  If UPLO = &#39;L&#39;, the leading
        /// N-by-N lower triangular part of A contains the lower
        /// triangular part of the matrix A, and the strictly upper
        /// triangular part of A is not referenced.
        /// 
        /// On exit, if FACT = &#39;E&#39; and EQUED = &#39;Y&#39;, A is overwritten by
        /// diag(S)*A*diag(S).
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="af">
        /// [in,out] AF is DOUBLE PRECISION array, dimension (LDAF,N).
        /// If FACT = &#39;F&#39;, then AF is an input argument and on entry
        /// contains the block diagonal matrix D and the multipliers
        /// used to obtain the factor U or L from the factorization A =
        /// U*D*U**T or A = L*D*L**T as computed by DSYTRF.
        /// 
        /// If FACT = &#39;N&#39;, then AF is an output argument and on exit
        /// returns the block diagonal matrix D and the multipliers
        /// used to obtain the factor U or L from the factorization A =
        /// U*D*U**T or A = L*D*L**T.
        /// </param>
        /// <param name="ldaf">
        /// [in] LDAF is INTEGER.
        /// The leading dimension of the array AF.  LDAF &gt;= max(1,N).
        /// </param>
        /// <param name="ipiv">
        /// [in,out] IPIV is INTEGER array, dimension (N).
        /// If FACT = &#39;F&#39;, then IPIV is an input argument and on entry
        /// contains details of the interchanges and the block
        /// structure of D, as determined by DSYTRF.  If IPIV(k) &gt; 0,
        /// then rows and columns k and IPIV(k) were interchanged and
        /// D(k,k) is a 1-by-1 diagonal block.  If UPLO = &#39;U&#39; and
        /// IPIV(k) = IPIV(k-1) &lt; 0, then rows and columns k-1 and
        /// -IPIV(k) were interchanged and D(k-1:k,k-1:k) is a 2-by-2
        /// diagonal block.  If UPLO = &#39;L&#39; and IPIV(k) = IPIV(k+1) &lt; 0,
        /// then rows and columns k+1 and -IPIV(k) were interchanged
        /// and D(k:k+1,k:k+1) is a 2-by-2 diagonal block.
        /// 
        /// If FACT = &#39;N&#39;, then IPIV is an output argument and on exit
        /// contains details of the interchanges and the block
        /// structure of D, as determined by DSYTRF.
        /// </param>
        /// <param name="equed">
        /// [in,out] EQUED is CHARACTER*1.
        /// Specifies the form of equilibration that was done.
        /// = &#39;N&#39;:  No equilibration (always true if FACT = &#39;N&#39;).
        /// = &#39;Y&#39;:  Both row and column equilibration, i.e., A has been
        /// replaced by diag(S) * A * diag(S).
        /// EQUED is an input argument if FACT = &#39;F&#39;; otherwise, it is an
        /// output argument.
        /// </param>
        /// <param name="s">
        /// [in,out] S is DOUBLE PRECISION array, dimension (N).
        /// The scale factors for A.  If EQUED = &#39;Y&#39;, A is multiplied on
        /// the left and right by diag(S).  S is an input argument if FACT =
        /// &#39;F&#39;; otherwise, S is an output argument.  If FACT = &#39;F&#39; and EQUED
        /// = &#39;Y&#39;, each element of S must be positive.  If S is output, each
        /// element of S is a power of the radix. If S is input, each element
        /// of S should be a power of the radix to ensure a reliable solution
        /// and error estimates. Scaling by powers of the radix does not cause
        /// rounding errors unless the result underflows or overflows.
        /// Rounding errors during scaling lead to refining with a matrix that
        /// is not equivalent to the input matrix, producing error estimates
        /// that may not be reliable.
        /// </param>
        /// <param name="b">
        /// [in,out] B is DOUBLE PRECISION array, dimension (LDB,NRHS).
        /// On entry, the N-by-NRHS right hand side matrix B.
        /// On exit,
        /// if EQUED = &#39;N&#39;, B is not modified;
        /// if EQUED = &#39;Y&#39;, B is overwritten by diag(S)*B;
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B.  LDB &gt;= max(1,N).
        /// </param>
        /// <param name="x">
        /// [out] X is DOUBLE PRECISION array, dimension (LDX,NRHS).
        /// If INFO = 0, the N-by-NRHS solution matrix X to the original
        /// system of equations.  Note that A and B are modified on exit if
        /// EQUED .ne. &#39;N&#39;, and the solution to the equilibrated system is
        /// inv(diag(S))*X.
        /// </param>
        /// <param name="ldx">
        /// [in] LDX is INTEGER.
        /// The leading dimension of the array X.  LDX &gt;= max(1,N).
        /// </param>
        /// <param name="rcond">
        /// [out] RCOND is DOUBLE PRECISION.
        /// Reciprocal scaled condition number.  This is an estimate of the
        /// reciprocal Skeel condition number of the matrix A after
        /// equilibration (if done).  If this is less than the machine
        /// precision (in particular, if it is zero), the matrix is singular
        /// to working precision.  Note that the error may still be small even
        /// if this number is very small and the matrix appears ill-
        /// conditioned.
        /// </param>
        /// <param name="rpvgrw">
        /// [out] RPVGRW is DOUBLE PRECISION.
        /// Reciprocal pivot growth.  On exit, this contains the reciprocal
        /// pivot growth factor norm(A)/norm(U). The &quot;max absolute element&quot;
        /// norm is used.  If this is much less than 1, then the stability of
        /// the LU factorization of the (equilibrated) matrix A could be poor.
        /// This also means that the solution X, estimated condition numbers,
        /// and error bounds could be unreliable. If factorization fails with
        /// 0&lt;INFO&lt;=N, then this contains the reciprocal pivot growth factor
        /// for the leading INFO columns of A.
        /// </param>
        /// <param name="berr">
        /// [out] BERR is DOUBLE PRECISION array, dimension (NRHS).
        /// Componentwise relative backward error.  This is the
        /// componentwise relative backward error of each solution vector X(j)
        /// (i.e., the smallest relative change in any element of A or B that
        /// makes X(j) an exact solution).
        /// </param>
        /// <param name="nErrBnds">
        /// [in] N_ERR_BNDS is INTEGER.
        /// Number of error bounds to return for each right hand side
        /// and each type (normwise or componentwise).  See ERR_BNDS_NORM and
        /// ERR_BNDS_COMP below.
        /// </param>
        /// <param name="errBndsNorm">
        /// [out] ERR_BNDS_NORM is DOUBLE PRECISION array, dimension (NRHS, N_ERR_BNDS).
        /// For each right-hand side, this array contains information about
        /// various error bounds and condition numbers corresponding to the
        /// normwise relative error, which is defined as follows:
        /// 
        /// Normwise relative error in the ith solution vector:
        /// max_j (abs(XTRUE(j,i) - X(j,i)))
        /// ------------------------------
        /// max_j abs(X(j,i))
        /// 
        /// The array is indexed by the type of error information as described
        /// below. There currently are up to three pieces of information
        /// returned.
        /// 
        /// The first index in ERR_BNDS_NORM(i,:) corresponds to the ith
        /// right-hand side.
        /// 
        /// The second index in ERR_BNDS_NORM(:,err) contains the following
        /// three fields:
        /// err = 1 &quot;Trust/don&#39;t trust&quot; boolean. Trust the answer if the
        /// reciprocal condition number is less than the threshold
        /// sqrt(n) * dlamch(&#39;Epsilon&#39;).
        /// 
        /// err = 2 &quot;Guaranteed&quot; error bound: The estimated forward error,
        /// almost certainly within a factor of 10 of the true error
        /// so long as the next entry is greater than the threshold
        /// sqrt(n) * dlamch(&#39;Epsilon&#39;). This error bound should only
        /// be trusted if the previous boolean is true.
        /// 
        /// err = 3  Reciprocal condition number: Estimated normwise
        /// reciprocal condition number.  Compared with the threshold
        /// sqrt(n) * dlamch(&#39;Epsilon&#39;) to determine if the error
        /// estimate is &quot;guaranteed&quot;. These reciprocal condition
        /// numbers are 1 / (norm(Z^{-1},inf) * norm(Z,inf)) for some
        /// appropriately scaled matrix Z.
        /// Let Z = S*A, where S scales each row by a power of the
        /// radix so all absolute row sums of Z are approximately 1.
        /// 
        /// See Lapack Working Note 165 for further details and extra
        /// cautions.
        /// </param>
        /// <param name="errBndsComp">
        /// [out] ERR_BNDS_COMP is DOUBLE PRECISION array, dimension (NRHS, N_ERR_BNDS).
        /// For each right-hand side, this array contains information about
        /// various error bounds and condition numbers corresponding to the
        /// componentwise relative error, which is defined as follows:
        /// 
        /// Componentwise relative error in the ith solution vector:
        /// abs(XTRUE(j,i) - X(j,i))
        /// max_j ----------------------
        /// abs(X(j,i))
        /// 
        /// The array is indexed by the right-hand side i (on which the
        /// componentwise relative error depends), and the type of error
        /// information as described below. There currently are up to three
        /// pieces of information returned for each right-hand side. If
        /// componentwise accuracy is not requested (PARAMS(3) = 0.0), then
        /// ERR_BNDS_COMP is not accessed.  If N_ERR_BNDS &lt; 3, then at most
        /// the first (:,N_ERR_BNDS) entries are returned.
        /// 
        /// The first index in ERR_BNDS_COMP(i,:) corresponds to the ith
        /// right-hand side.
        /// 
        /// The second index in ERR_BNDS_COMP(:,err) contains the following
        /// three fields:
        /// err = 1 &quot;Trust/don&#39;t trust&quot; boolean. Trust the answer if the
        /// reciprocal condition number is less than the threshold
        /// sqrt(n) * dlamch(&#39;Epsilon&#39;).
        /// 
        /// err = 2 &quot;Guaranteed&quot; error bound: The estimated forward error,
        /// almost certainly within a factor of 10 of the true error
        /// so long as the next entry is greater than the threshold
        /// sqrt(n) * dlamch(&#39;Epsilon&#39;). This error bound should only
        /// be trusted if the previous boolean is true.
        /// 
        /// err = 3  Reciprocal condition number: Estimated componentwise
        /// reciprocal condition number.  Compared with the threshold
        /// sqrt(n) * dlamch(&#39;Epsilon&#39;) to determine if the error
        /// estimate is &quot;guaranteed&quot;. These reciprocal condition
        /// numbers are 1 / (norm(Z^{-1},inf) * norm(Z,inf)) for some
        /// appropriately scaled matrix Z.
        /// Let Z = S*(A*diag(x)), where x is the solution for the
        /// current right-hand side and S scales each row of
        /// A*diag(x) by a power of the radix so all absolute row
        /// sums of Z are approximately 1.
        /// 
        /// See Lapack Working Note 165 for further details and extra
        /// cautions.
        /// </param>
        /// <param name="nparams">
        /// [in] NPARAMS is INTEGER.
        /// Specifies the number of parameters set in PARAMS.  If &lt;= 0, the
        /// PARAMS array is never referenced and default values are used.
        /// </param>
        /// <param name="params">
        /// [in,out] PARAMS is DOUBLE PRECISION array, dimension (NPARAMS).
        /// Specifies algorithm parameters.  If an entry is &lt; 0.0, then
        /// that entry will be filled with default value used for that
        /// parameter.  Only positions up to NPARAMS are accessed; defaults
        /// are used for higher-numbered parameters.
        /// 
        /// PARAMS(LA_LINRX_ITREF_I = 1) : Whether to perform iterative
        /// refinement or not.
        /// Default: 1.0D+0
        /// = 0.0:  No refinement is performed, and no error bounds are
        /// computed.
        /// = 1.0:  Use the extra-precise refinement algorithm.
        /// (other values are reserved for future use)
        /// 
        /// PARAMS(LA_LINRX_ITHRESH_I = 2) : Maximum number of residual
        /// computations allowed for refinement.
        /// Default: 10
        /// Aggressive: Set to 100 to permit convergence using approximate
        /// factorizations or factorizations other than LU. If
        /// the factorization uses a technique other than
        /// Gaussian elimination, the guarantees in
        /// err_bnds_norm and err_bnds_comp may no longer be
        /// trustworthy.
        /// 
        /// PARAMS(LA_LINRX_CWISE_I = 3) : Flag determining if the code
        /// will attempt to find a solution with small componentwise
        /// relative error in the double-precision algorithm.  Positive
        /// is true, 0.0 is false.
        /// Default: 1.0 (attempt componentwise convergence)
        /// </param>
        /// <returns>
        /// = 0:  Successful exit. The solution to every right-hand side is
        /// guaranteed.
        /// &lt; 0:  If INFO = -i, the i-th argument had an illegal value
        /// &gt; 0 and &lt;= N:  U(INFO,INFO) is exactly zero.  The factorization
        /// has been completed, but the factor U is exactly singular, so
        /// the solution and error bounds could not be computed. RCOND = 0
        /// is returned.
        /// = N+J: The solution corresponding to the Jth right-hand side is
        /// not guaranteed. The solutions corresponding to other right-
        /// hand sides K with K &gt; J may not be guaranteed as well, but
        /// only the first such right-hand side is reported. If a small
        /// componentwise error is not requested (PARAMS(3) = 0.0) then
        /// the Jth right-hand side is the first with a normwise error
        /// bound that is not guaranteed (the smallest J such
        /// that ERR_BNDS_NORM(J,1) = 0.0). By default (PARAMS(3) = 1.0)
        /// the Jth right-hand side is the first with either a normwise or
        /// componentwise error bound that is not guaranteed (the smallest
        /// J such that either ERR_BNDS_NORM(J,1) = 0.0 or
        /// ERR_BNDS_COMP(J,1) = 0.0). See the definition of
        /// ERR_BNDS_NORM(:,1) and ERR_BNDS_COMP(:,1). To get information
        /// about all of the right-hand sides check ERR_BNDS_NORM or
        /// ERR_BNDS_COMP.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dsysvxx", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dsysvxx(
            MatrixLayout matrixLayout,
            char fact,
            char uplo,
            int n,
            int nrhs,
            double* a,
            int lda,
            double* af,
            int ldaf,
            int* ipiv,
            ref char equed,
            double* s,
            double* b,
            int ldb,
            double* x,
            int ldx,
            double* rcond,
            double* rpvgrw,
            double* berr,
            int nErrBnds,
            double* errBndsNorm,
            double* errBndsComp,
            int nparams,
            double* @params);
    }
}
