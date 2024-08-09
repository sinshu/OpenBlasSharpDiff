using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZGGLSE solves the linear equality-constrained least squares (LSE)
        /// problem:
        /// </para>
        /// <para>
        ///         minimize || c - A*x ||_2   subject to   B*x = d
        /// </para>
        /// <para>
        /// where A is an M-by-N matrix, B is a P-by-N matrix, c is a given
        /// M-vector, and d is a given P-vector. It is assumed that
        /// P &lt;= N &lt;= M+P, and
        /// </para>
        /// <para>
        ///          rank(B) = P and  rank( (A) ) = N.
        ///                               ( (B) )
        /// </para>
        /// <para>
        /// These conditions ensure that the LSE problem has a unique solution,
        /// which is obtained using a generalized RQ factorization of the
        /// matrices (B, A) given by
        /// </para>
        /// <para>
        ///    B = (0 R)*Q,   A = Z*T*Q.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of rows of the matrix A.  M &gt;= 0.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of columns of the matrices A and B. N &gt;= 0.
        /// </param>
        /// <param name="p">
        /// [in] P is INTEGER.
        /// The number of rows of the matrix B. 0 &lt;= P &lt;= N &lt;= M+P.
        /// </param>
        /// <param name="a">
        /// [in,out] A is COMPLEX*16 array, dimension (LDA,N).
        /// On entry, the M-by-N matrix A.
        /// On exit, the elements on and above the diagonal of the array
        /// contain the min(M,N)-by-N upper trapezoidal matrix T.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A. LDA &gt;= max(1,M).
        /// </param>
        /// <param name="b">
        /// [in,out] B is COMPLEX*16 array, dimension (LDB,N).
        /// On entry, the P-by-N matrix B.
        /// On exit, the upper triangle of the subarray B(1:P,N-P+1:N)
        /// contains the P-by-P upper triangular matrix R.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B. LDB &gt;= max(1,P).
        /// </param>
        /// <param name="c">
        /// [in,out] C is COMPLEX*16 array, dimension (M).
        /// On entry, C contains the right hand side vector for the
        /// least squares part of the LSE problem.
        /// On exit, the residual sum of squares for the solution
        /// is given by the sum of squares of elements N-P+1 to M of
        /// vector C.
        /// </param>
        /// <param name="d">
        /// [in,out] D is COMPLEX*16 array, dimension (P).
        /// On entry, D contains the right hand side vector for the
        /// constrained equation.
        /// On exit, D is destroyed.
        /// </param>
        /// <param name="x">
        /// [out] X is COMPLEX*16 array, dimension (N).
        /// On exit, X is the solution of the LSE problem.
        /// </param>
        /// <returns>
        /// = 0:  successful exit.
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value.
        /// = 1:  the upper triangular factor R associated with B in the
        /// generalized RQ factorization of the pair (B, A) is
        /// singular, so that rank(B) &lt; P; the least squares
        /// solution could not be computed.
        /// = 2:  the (N-P) by (N-P) part of the upper trapezoidal factor
        /// T associated with A in the generalized RQ factorization
        /// of the pair (B, A) is singular, so that
        /// rank( (A) ) &lt; N; the least squares solution could not
        /// ( (B) )
        /// be computed.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zgglse", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zgglse(
            MatrixLayout matrixLayout,
            int m,
            int n,
            int p,
            Complex* a,
            int lda,
            Complex* b,
            int ldb,
            Complex* c,
            Complex* d,
            Complex* x);
    }
}
