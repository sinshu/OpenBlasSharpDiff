using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Blas
    {
        /// <summary>
        /// <para>
        /// ZGERU  performs the rank 1 operation
        /// </para>
        /// <para>
        ///    A := alpha*x*y**T + A,
        /// </para>
        /// <para>
        /// where alpha is a scalar, x is an m element vector, y is an n element
        /// vector and A is an m by n matrix.
        /// </para>
        /// </summary>
        /// <param name="order">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// On entry, M specifies the number of rows of the matrix A.
        /// M must be at least zero.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// On entry, N specifies the number of columns of the matrix A.
        /// N must be at least zero.
        /// </param>
        /// <param name="alpha">
        /// [in] ALPHA is COMPLEX*16.
        /// On entry, ALPHA specifies the scalar alpha.
        /// </param>
        /// <param name="x">
        /// [in] X is COMPLEX*16 array, dimension at least.
        /// ( 1 + ( m - 1 )*abs( INCX ) ).
        /// Before entry, the incremented array X must contain the m
        /// element vector x.
        /// </param>
        /// <param name="incx">
        /// [in] INCX is INTEGER.
        /// On entry, INCX specifies the increment for the elements of
        /// X. INCX must not be zero.
        /// </param>
        /// <param name="y">
        /// [in] Y is COMPLEX*16 array, dimension at least.
        /// ( 1 + ( n - 1 )*abs( INCY ) ).
        /// Before entry, the incremented array Y must contain the n
        /// element vector y.
        /// </param>
        /// <param name="incy">
        /// [in] INCY is INTEGER.
        /// On entry, INCY specifies the increment for the elements of
        /// Y. INCY must not be zero.
        /// </param>
        /// <param name="a">
        /// [in,out] A is COMPLEX*16 array, dimension ( LDA, N ).
        /// Before entry, the leading m by n part of the array A must
        /// contain the matrix of coefficients. On exit, A is
        /// overwritten by the updated matrix.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// On entry, LDA specifies the first dimension of A as declared
        /// in the calling (sub) program. LDA must be at least
        /// max( 1, m ).
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "cblas_zgeru", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe void Zgeru(
            Order order,
            int m,
            int n,
            void* alpha,
            void* x,
            int incx,
            void* y,
            int incy,
            void* a,
            int lda);
    }
}
