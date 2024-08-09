using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DSPOSV computes the solution to a real system of linear equations
        ///    A * X = B,
        /// where A is an N-by-N symmetric positive definite matrix and X and B
        /// are N-by-NRHS matrices.
        /// </para>
        /// <para>
        /// DSPOSV first attempts to factorize the matrix in SINGLE PRECISION
        /// and use this factorization within an iterative refinement procedure
        /// to produce a solution with DOUBLE PRECISION normwise backward error
        /// quality (see below). If the approach fails the method switches to a
        /// DOUBLE PRECISION factorization and solve.
        /// </para>
        /// <para>
        /// The iterative refinement is not going to be a winning strategy if
        /// the ratio SINGLE PRECISION performance over DOUBLE PRECISION
        /// performance is too small. A reasonable strategy should take the
        /// number of right-hand sides and the size of the matrix into account.
        /// This might be done with a call to ILAENV in the future. Up to now, we
        /// always try iterative refinement.
        /// </para>
        /// <para>
        /// The iterative refinement process is stopped if
        ///     ITER &gt; ITERMAX
        /// or for all the RHS we have:
        ///     RNRM &lt; SQRT(N)*XNRM*ANRM*EPS*BWDMAX
        /// where
        ///     o ITER is the number of the current iteration in the iterative
        ///       refinement process
        ///     o RNRM is the infinity-norm of the residual
        ///     o XNRM is the infinity-norm of the solution
        ///     o ANRM is the infinity-operator-norm of the matrix A
        ///     o EPS is the machine epsilon returned by DLAMCH(&#39;Epsilon&#39;)
        /// The value ITERMAX and BWDMAX are fixed to 30 and 1.0D+00
        /// respectively.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
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
        /// of the matrix B.  NRHS &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is DOUBLE PRECISION array,.
        /// dimension (LDA,N)
        /// On entry, the symmetric matrix A.  If UPLO = &#39;U&#39;, the leading
        /// N-by-N upper triangular part of A contains the upper
        /// triangular part of the matrix A, and the strictly lower
        /// triangular part of A is not referenced.  If UPLO = &#39;L&#39;, the
        /// leading N-by-N lower triangular part of A contains the lower
        /// triangular part of the matrix A, and the strictly upper
        /// triangular part of A is not referenced.
        /// On exit, if iterative refinement has been successfully used
        /// (INFO = 0 and ITER &gt;= 0, see description below), then A is
        /// unchanged, if double precision factorization has been used
        /// (INFO = 0 and ITER &lt; 0, see description below), then the
        /// array A contains the factor U or L from the Cholesky
        /// factorization A = U**T*U or A = L*L**T.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="b">
        /// [in] B is DOUBLE PRECISION array, dimension (LDB,NRHS).
        /// The N-by-NRHS right hand side matrix B.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B.  LDB &gt;= max(1,N).
        /// </param>
        /// <param name="x">
        /// [out] X is DOUBLE PRECISION array, dimension (LDX,NRHS).
        /// If INFO = 0, the N-by-NRHS solution matrix X.
        /// </param>
        /// <param name="ldx">
        /// [in] LDX is INTEGER.
        /// The leading dimension of the array X.  LDX &gt;= max(1,N).
        /// </param>
        /// <param name="iter">
        /// [out] ITER is INTEGER.
        /// &lt; 0: iterative refinement has failed, double precision
        /// factorization has been performed
        /// -1 : the routine fell back to full precision for
        /// implementation- or machine-specific reasons
        /// -2 : narrowing the precision induced an overflow,
        /// the routine fell back to full precision
        /// -3 : failure of SPOTRF
        /// -31: stop the iterative refinement after the 30th
        /// iterations
        /// &gt; 0: iterative refinement has been successfully used.
        /// Returns the number of iterations
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  if INFO = i, the leading principal minor of order i
        /// of (DOUBLE PRECISION) A is not positive, so the
        /// factorization could not be completed, and the solution
        /// has not been computed.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dsposv", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dsposv(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            int nrhs,
            double* a,
            int lda,
            double* b,
            int ldb,
            double* x,
            int ldx,
            int* iter);
    }
}
