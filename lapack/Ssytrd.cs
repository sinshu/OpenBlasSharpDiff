using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// SSYTRD reduces a real symmetric matrix A to real symmetric
        /// tridiagonal form T by an orthogonal similarity transformation:
        /// Q**T * A * Q = T.
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
        /// <param name="a">
        /// [in,out] A is REAL array, dimension (LDA,N).
        /// On entry, the symmetric matrix A.  If UPLO = &#39;U&#39;, the leading
        /// N-by-N upper triangular part of A contains the upper
        /// triangular part of the matrix A, and the strictly lower
        /// triangular part of A is not referenced.  If UPLO = &#39;L&#39;, the
        /// leading N-by-N lower triangular part of A contains the lower
        /// triangular part of the matrix A, and the strictly upper
        /// triangular part of A is not referenced.
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
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="d">
        /// [out] D is REAL array, dimension (N).
        /// The diagonal elements of the tridiagonal matrix T:
        /// D(i) = A(i,i).
        /// </param>
        /// <param name="e">
        /// [out] E is REAL array, dimension (N-1).
        /// The off-diagonal elements of the tridiagonal matrix T:
        /// E(i) = A(i,i+1) if UPLO = &#39;U&#39;, E(i) = A(i+1,i) if UPLO = &#39;L&#39;.
        /// </param>
        /// <param name="tau">
        /// [out] TAU is REAL array, dimension (N-1).
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
        ///  v(i+1:n) = 0 and v(i) = 1; v(1:i-1) is stored on exit in
        ///  A(1:i-1,i+1), and tau in TAU(i).
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
        ///  v(1:i) = 0 and v(i+1) = 1; v(i+2:n) is stored on exit in A(i+2:n,i),
        ///  and tau in TAU(i).
        /// </para>
        /// <para>
        ///  The contents of A on exit are illustrated by the following examples
        ///  with n = 5:
        /// </para>
        /// <para>
        ///  if UPLO = &#39;U&#39;:                       if UPLO = &#39;L&#39;:
        /// </para>
        /// <para>
        ///    (  d   e   v2  v3  v4 )              (  d                  )
        ///    (      d   e   v3  v4 )              (  e   d              )
        ///    (          d   e   v4 )              (  v1  e   d          )
        ///    (              d   e  )              (  v1  v2  e   d      )
        ///    (                  d  )              (  v1  v2  v3  e   d  )
        /// </para>
        /// <para>
        ///  where d and e denote diagonal and off-diagonal elements of T, and vi
        ///  denotes an element of the vector defining H(i).
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_ssytrd", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Ssytrd(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            float* a,
            int lda,
            float* d,
            float* e,
            float* tau);
    }
}
