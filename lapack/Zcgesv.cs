using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZCGESV computes the solution to a complex system of linear equations
        ///    A * X = B,
        /// where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
        /// </para>
        /// <para>
        /// ZCGESV first attempts to factorize the matrix in COMPLEX and use this
        /// factorization within an iterative refinement procedure to produce a
        /// solution with COMPLEX*16 normwise backward error quality (see below).
        /// If the approach fails the method switches to a COMPLEX*16
        /// factorization and solve.
        /// </para>
        /// <para>
        /// The iterative refinement is not going to be a winning strategy if
        /// the ratio COMPLEX performance over COMPLEX*16 performance is too
        /// small. A reasonable strategy should take the number of right-hand
        /// sides and the size of the matrix into account. This might be done
        /// with a call to ILAENV in the future. Up to now, we always try
        /// iterative refinement.
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
        /// [in,out] A is COMPLEX*16 array,.
        /// dimension (LDA,N)
        /// On entry, the N-by-N coefficient matrix A.
        /// On exit, if iterative refinement has been successfully used
        /// (INFO = 0 and ITER &gt;= 0, see description below), then A is
        /// unchanged, if double precision factorization has been used
        /// (INFO = 0 and ITER &lt; 0, see description below), then the
        /// array A contains the factors L and U from the factorization
        /// A = P*L*U; the unit diagonal elements of L are not stored.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="ipiv">
        /// [out] IPIV is INTEGER array, dimension (N).
        /// The pivot indices that define the permutation matrix P;
        /// row i of the matrix was interchanged with row IPIV(i).
        /// Corresponds either to the single precision factorization
        /// (if INFO = 0 and ITER &gt;= 0) or the double precision
        /// factorization (if INFO = 0 and ITER &lt; 0).
        /// </param>
        /// <param name="b">
        /// [in] B is COMPLEX*16 array, dimension (LDB,NRHS).
        /// The N-by-NRHS right hand side matrix B.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B.  LDB &gt;= max(1,N).
        /// </param>
        /// <param name="x">
        /// [out] X is COMPLEX*16 array, dimension (LDX,NRHS).
        /// If INFO = 0, the N-by-NRHS solution matrix X.
        /// </param>
        /// <param name="ldx">
        /// [in] LDX is INTEGER.
        /// The leading dimension of the array X.  LDX &gt;= max(1,N).
        /// </param>
        /// <param name="iter">
        /// [out] ITER is INTEGER.
        /// &lt; 0: iterative refinement has failed, COMPLEX*16
        /// factorization has been performed
        /// -1 : the routine fell back to full precision for
        /// implementation- or machine-specific reasons
        /// -2 : narrowing the precision induced an overflow,
        /// the routine fell back to full precision
        /// -3 : failure of CGETRF
        /// -31: stop the iterative refinement after the 30th
        /// iterations
        /// &gt; 0: iterative refinement has been successfully used.
        /// Returns the number of iterations
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  if INFO = i, U(i,i) computed in COMPLEX*16 is exactly
        /// zero.  The factorization has been completed, but the
        /// factor U is exactly singular, so the solution
        /// could not be computed.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zcgesv", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zcgesv(
            MatrixLayout matrixLayout,
            int n,
            int nrhs,
            Complex* a,
            int lda,
            int* ipiv,
            Complex* b,
            int ldb,
            Complex* x,
            int ldx,
            int* iter);
    }
}
