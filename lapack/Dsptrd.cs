using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DSPTRD reduces a real symmetric matrix A stored in packed form to
        /// symmetric tridiagonal form T by an orthogonal similarity
        /// transformation: Q**T * A * Q = T.
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
        /// The order of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="ap">
        /// [in,out] AP is DOUBLE PRECISION array, dimension (N*(N+1)/2).
        /// On entry, the upper or lower triangle of the symmetric matrix
        /// A, packed columnwise in a linear array.  The j-th column of A
        /// is stored in the array AP as follows:
        /// if UPLO = &#39;U&#39;, AP(i + (j-1)*j/2) = A(i,j) for 1&lt;=i&lt;=j;
        /// if UPLO = &#39;L&#39;, AP(i + (j-1)*(2*n-j)/2) = A(i,j) for j&lt;=i&lt;=n.
        /// On exit, if UPLO = &#39;U&#39;, the diagonal and first superdiagonal
        /// of A are overwritten by the corresponding elements of the
        /// tridiagonal matrix T, and the elements above the first
        /// superdiagonal, with the array TAU, represent the orthogonal
        /// matrix Q as a product of elementary reflectors; if UPLO
        /// = &#39;L&#39;, the diagonal and first subdiagonal of A are over-
        /// written by the corresponding elements of the tridiagonal
        /// matrix T, and the elements below the first subdiagonal, with
        /// the array TAU, represent the orthogonal matrix Q as a product
        /// of elementary reflectors. See Further Details.
        /// </param>
        /// <param name="d">
        /// [out] D is DOUBLE PRECISION array, dimension (N).
        /// The diagonal elements of the tridiagonal matrix T:
        /// D(i) = A(i,i).
        /// </param>
        /// <param name="e">
        /// [out] E is DOUBLE PRECISION array, dimension (N-1).
        /// The off-diagonal elements of the tridiagonal matrix T:
        /// E(i) = A(i,i+1) if UPLO = &#39;U&#39;, E(i) = A(i+1,i) if UPLO = &#39;L&#39;.
        /// </param>
        /// <param name="tau">
        /// [out] TAU is DOUBLE PRECISION array, dimension (N-1).
        /// The scalar factors of the elementary reflectors (see Further
        /// Details).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        /// <remarks>
        /// <para>
        ///  If UPLO = &#39;U&#39;, the matrix Q is represented as a product of elementary
        ///  reflectors
        /// </para>
        /// <para>
        ///     Q = H(n-1) . . . H(2) H(1).
        /// </para>
        /// <para>
        ///  Each H(i) has the form
        /// </para>
        /// <para>
        ///     H(i) = I - tau * v * v**T
        /// </para>
        /// <para>
        ///  where tau is a real scalar, and v is a real vector with
        ///  v(i+1:n) = 0 and v(i) = 1; v(1:i-1) is stored on exit in AP,
        ///  overwriting A(1:i-1,i+1), and tau is stored in TAU(i).
        /// </para>
        /// <para>
        ///  If UPLO = &#39;L&#39;, the matrix Q is represented as a product of elementary
        ///  reflectors
        /// </para>
        /// <para>
        ///     Q = H(1) H(2) . . . H(n-1).
        /// </para>
        /// <para>
        ///  Each H(i) has the form
        /// </para>
        /// <para>
        ///     H(i) = I - tau * v * v**T
        /// </para>
        /// <para>
        ///  where tau is a real scalar, and v is a real vector with
        ///  v(1:i) = 0 and v(i+1) = 1; v(i+2:n) is stored on exit in AP,
        ///  overwriting A(i+2:n,i), and tau is stored in TAU(i).
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dsptrd", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dsptrd(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            double* ap,
            double* d,
            double* e,
            double* tau);
    }
}
