﻿using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        ///    DGERFSX improves the computed solution to a system of linear
        ///    equations and provides error bounds and backward error estimates
        ///    for the solution.  In addition to normwise error bound, the code
        ///    provides maximum componentwise error bound if possible.  See
        ///    comments for ERR_BNDS_NORM and ERR_BNDS_COMP for details of the
        ///    error bounds.
        /// </para>
        /// <para>
        ///    The original system of linear equations may have been equilibrated
        ///    before calling this routine, as described by arguments EQUED, R
        ///    and C below. In this case, the solution and error bounds returned
        ///    are for the original unequilibrated system.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="trans">
        /// [in] TRANS is CHARACTER*1.
        /// Specifies the form of the system of equations:
        /// = &#39;N&#39;:  A * X = B     (No transpose)
        /// = &#39;T&#39;:  A**T * X = B  (Transpose)
        /// = &#39;C&#39;:  A**H * X = B  (Conjugate transpose = Transpose)
        /// </param>
        /// <param name="equed">
        /// [in] EQUED is CHARACTER*1.
        /// Specifies the form of equilibration that was done to A
        /// before calling this routine. This is needed to compute
        /// the solution and error bounds correctly.
        /// = &#39;N&#39;:  No equilibration
        /// = &#39;R&#39;:  Row equilibration, i.e., A has been premultiplied by
        /// diag(R).
        /// = &#39;C&#39;:  Column equilibration, i.e., A has been postmultiplied
        /// by diag(C).
        /// = &#39;B&#39;:  Both row and column equilibration, i.e., A has been
        /// replaced by diag(R) * A * diag(C).
        /// The right hand side B has been changed accordingly.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="nrhs">
        /// [in] NRHS is INTEGER.
        /// The number of right hand sides, i.e., the number of columns
        /// of the matrices B and X.  NRHS &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in] A is DOUBLE PRECISION array, dimension (LDA,N).
        /// The original N-by-N matrix A.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="af">
        /// [in] AF is DOUBLE PRECISION array, dimension (LDAF,N).
        /// The factors L and U from the factorization A = P*L*U
        /// as computed by DGETRF.
        /// </param>
        /// <param name="ldaf">
        /// [in] LDAF is INTEGER.
        /// The leading dimension of the array AF.  LDAF &gt;= max(1,N).
        /// </param>
        /// <param name="ipiv">
        /// [in] IPIV is INTEGER array, dimension (N).
        /// The pivot indices from DGETRF; for 1&lt;=i&lt;=N, row i of the
        /// matrix was interchanged with row IPIV(i).
        /// </param>
        /// <param name="r">
        /// [in] R is DOUBLE PRECISION array, dimension (N).
        /// The row scale factors for A.  If EQUED = &#39;R&#39; or &#39;B&#39;, A is
        /// multiplied on the left by diag(R); if EQUED = &#39;N&#39; or &#39;C&#39;, R
        /// is not accessed.
        /// If R is accessed, each element of R should be a power of the radix
        /// to ensure a reliable solution and error estimates. Scaling by
        /// powers of the radix does not cause rounding errors unless the
        /// result underflows or overflows. Rounding errors during scaling
        /// lead to refining with a matrix that is not equivalent to the
        /// input matrix, producing error estimates that may not be
        /// reliable.
        /// </param>
        /// <param name="c">
        /// [in] C is DOUBLE PRECISION array, dimension (N).
        /// The column scale factors for A.  If EQUED = &#39;C&#39; or &#39;B&#39;, A is
        /// multiplied on the right by diag(C); if EQUED = &#39;N&#39; or &#39;R&#39;, C
        /// is not accessed.
        /// If C is accessed, each element of C should be a power of the radix
        /// to ensure a reliable solution and error estimates. Scaling by
        /// powers of the radix does not cause rounding errors unless the
        /// result underflows or overflows. Rounding errors during scaling
        /// lead to refining with a matrix that is not equivalent to the
        /// input matrix, producing error estimates that may not be
        /// reliable.
        /// </param>
        /// <param name="b">
        /// [in] B is DOUBLE PRECISION array, dimension (LDB,NRHS).
        /// The right hand side matrix B.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B.  LDB &gt;= max(1,N).
        /// </param>
        /// <param name="x">
        /// [in,out] X is DOUBLE PRECISION array, dimension (LDX,NRHS).
        /// On entry, the solution matrix X, as computed by DGETRS.
        /// On exit, the improved solution matrix X.
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
        /// = 1.0:  Use the double-precision refinement algorithm,
        /// possibly with doubled-single computations if the
        /// compilation environment does not support DOUBLE
        /// PRECISION.
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
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dgerfsx", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dgerfsx(
            MatrixLayout matrixLayout,
            char trans,
            char equed,
            int n,
            int nrhs,
            double* a,
            int lda,
            double* af,
            int ldaf,
            int* ipiv,
            double* r,
            double* c,
            double* b,
            int ldb,
            double* x,
            int ldx,
            double* rcond,
            double* berr,
            int nErrBnds,
            double* errBndsNorm,
            double* errBndsComp,
            int nparams,
            double* @params);
    }
}
