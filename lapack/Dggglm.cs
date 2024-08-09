using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DGGGLM solves a general Gauss-Markov linear model (GLM) problem:
        /// </para>
        /// <para>
        ///         minimize || y ||_2   subject to   d = A*x + B*y
        ///             x
        /// </para>
        /// <para>
        /// where A is an N-by-M matrix, B is an N-by-P matrix, and d is a
        /// given N-vector. It is assumed that M &lt;= N &lt;= M+P, and
        /// </para>
        /// <para>
        ///            rank(A) = M    and    rank( A B ) = N.
        /// </para>
        /// <para>
        /// Under these assumptions, the constrained equation is always
        /// consistent, and there is a unique solution x and a minimal 2-norm
        /// solution y, which is obtained using a generalized QR factorization
        /// of the matrices (A, B) given by
        /// </para>
        /// <para>
        ///    A = Q*(R),   B = Q*T*Z.
        ///          (0)
        /// </para>
        /// <para>
        /// In particular, if matrix B is square nonsingular, then the problem
        /// GLM is equivalent to the following weighted linear least squares
        /// problem
        /// </para>
        /// <para>
        ///              minimize || inv(B)*(d-A*x) ||_2
        ///                  x
        /// </para>
        /// <para>
        /// where inv(B) denotes the inverse of B.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of rows of the matrices A and B.  N &gt;= 0.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of columns of the matrix A.  0 &lt;= M &lt;= N.
        /// </param>
        /// <param name="p">
        /// [in] P is INTEGER.
        /// The number of columns of the matrix B.  P &gt;= N-M.
        /// </param>
        /// <param name="a">
        /// [in,out] A is DOUBLE PRECISION array, dimension (LDA,M).
        /// On entry, the N-by-M matrix A.
        /// On exit, the upper triangular part of the array A contains
        /// the M-by-M upper triangular matrix R.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A. LDA &gt;= max(1,N).
        /// </param>
        /// <param name="b">
        /// [in,out] B is DOUBLE PRECISION array, dimension (LDB,P).
        /// On entry, the N-by-P matrix B.
        /// On exit, if N &lt;= P, the upper triangle of the subarray
        /// B(1:N,P-N+1:P) contains the N-by-N upper triangular matrix T;
        /// if N &gt; P, the elements on and above the (N-P)th subdiagonal
        /// contain the N-by-P upper trapezoidal matrix T.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B. LDB &gt;= max(1,N).
        /// </param>
        /// <param name="d">
        /// [in,out] D is DOUBLE PRECISION array, dimension (N).
        /// On entry, D is the left hand side of the GLM equation.
        /// On exit, D is destroyed.
        /// </param>
        /// <param name="x">
        /// [out] X is DOUBLE PRECISION array, dimension (M).
        /// </param>
        /// <param name="y">
        /// [out] Y is DOUBLE PRECISION array, dimension (P).
        /// 
        /// On exit, X and Y are the solutions of the GLM problem.
        /// </param>
        /// <returns>
        /// = 0:  successful exit.
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value.
        /// = 1:  the upper triangular factor R associated with A in the
        /// generalized QR factorization of the pair (A, B) is
        /// singular, so that rank(A) &lt; M; the least squares
        /// solution could not be computed.
        /// = 2:  the bottom (N-M) by (N-M) part of the upper trapezoidal
        /// factor T associated with B in the generalized QR
        /// factorization of the pair (A, B) is singular, so that
        /// rank( A B ) &lt; N; the least squares solution could not
        /// be computed.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dggglm", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dggglm(
            MatrixLayout matrixLayout,
            int n,
            int m,
            int p,
            double* a,
            int lda,
            double* b,
            int ldb,
            double* d,
            double* x,
            double* y);
    }
}
