using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DGELQ2 computes an LQ factorization of a real m-by-n matrix A:
        /// </para>
        /// <para>
        ///    A = ( L 0 ) *  Q
        /// </para>
        /// <para>
        /// where:
        /// </para>
        /// <para>
        ///    Q is a n-by-n orthogonal matrix;
        ///    L is a lower-triangular m-by-m matrix;
        ///    0 is a m-by-(n-m) zero matrix, if m &lt; n.
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
        /// The number of columns of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is DOUBLE PRECISION array, dimension (LDA,N).
        /// On entry, the m by n matrix A.
        /// On exit, the elements on and below the diagonal of the array
        /// contain the m by min(m,n) lower trapezoidal matrix L (L is
        /// lower triangular if m &lt;= n); the elements above the diagonal,
        /// with the array TAU, represent the orthogonal matrix Q as a
        /// product of elementary reflectors (see Further Details).
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,M).
        /// </param>
        /// <param name="tau">
        /// [out] TAU is DOUBLE PRECISION array, dimension (min(M,N)).
        /// The scalar factors of the elementary reflectors (see Further
        /// Details).
        /// </param>
        /// <returns>
        /// = 0: successful exit
        /// &lt; 0: if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        /// <remarks>
        /// <para>
        ///  The matrix Q is represented as a product of elementary reflectors
        /// </para>
        /// <para>
        ///     Q = H(k) . . . H(2) H(1), where k = min(m,n).
        /// </para>
        /// <para>
        ///  Each H(i) has the form
        /// </para>
        /// <para>
        ///     H(i) = I - tau * v * v**T
        /// </para>
        /// <para>
        ///  where tau is a real scalar, and v is a real vector with
        ///  v(1:i-1) = 0 and v(i) = 1; v(i+1:n) is stored on exit in A(i,i+1:n),
        ///  and tau in TAU(i).
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dgelq2", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dgelq2(
            MatrixLayout matrixLayout,
            int m,
            int n,
            double* a,
            int lda,
            double* tau);
    }
}
