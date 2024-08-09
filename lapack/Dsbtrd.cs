using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DSBTRD reduces a real symmetric band matrix A to symmetric
        /// tridiagonal form T by an orthogonal similarity transformation:
        /// Q**T * A * Q = T.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="vect">
        /// [in] VECT is CHARACTER*1.
        /// = &#39;N&#39;:  do not form Q;
        /// = &#39;V&#39;:  form Q;
        /// = &#39;U&#39;:  update a matrix X, by forming X*Q.
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
        /// <param name="kd">
        /// [in] KD is INTEGER.
        /// The number of superdiagonals of the matrix A if UPLO = &#39;U&#39;,
        /// or the number of subdiagonals if UPLO = &#39;L&#39;.  KD &gt;= 0.
        /// </param>
        /// <param name="ab">
        /// [in,out] AB is DOUBLE PRECISION array, dimension (LDAB,N).
        /// On entry, the upper or lower triangle of the symmetric band
        /// matrix A, stored in the first KD+1 rows of the array.  The
        /// j-th column of A is stored in the j-th column of the array AB
        /// as follows:
        /// if UPLO = &#39;U&#39;, AB(kd+1+i-j,j) = A(i,j) for max(1,j-kd)&lt;=i&lt;=j;
        /// if UPLO = &#39;L&#39;, AB(1+i-j,j)    = A(i,j) for j&lt;=i&lt;=min(n,j+kd).
        /// On exit, the diagonal elements of AB are overwritten by the
        /// diagonal elements of the tridiagonal matrix T; if KD &gt; 0, the
        /// elements on the first superdiagonal (if UPLO = &#39;U&#39;) or the
        /// first subdiagonal (if UPLO = &#39;L&#39;) are overwritten by the
        /// off-diagonal elements of T; the rest of AB is overwritten by
        /// values generated during the reduction.
        /// </param>
        /// <param name="ldab">
        /// [in] LDAB is INTEGER.
        /// The leading dimension of the array AB.  LDAB &gt;= KD+1.
        /// </param>
        /// <param name="d">
        /// [out] D is DOUBLE PRECISION array, dimension (N).
        /// The diagonal elements of the tridiagonal matrix T.
        /// </param>
        /// <param name="e">
        /// [out] E is DOUBLE PRECISION array, dimension (N-1).
        /// The off-diagonal elements of the tridiagonal matrix T:
        /// E(i) = T(i,i+1) if UPLO = &#39;U&#39;; E(i) = T(i+1,i) if UPLO = &#39;L&#39;.
        /// </param>
        /// <param name="q">
        /// [in,out] Q is DOUBLE PRECISION array, dimension (LDQ,N).
        /// On entry, if VECT = &#39;U&#39;, then Q must contain an N-by-N
        /// matrix X; if VECT = &#39;N&#39; or &#39;V&#39;, then Q need not be set.
        /// 
        /// On exit:
        /// if VECT = &#39;V&#39;, Q contains the N-by-N orthogonal matrix Q;
        /// if VECT = &#39;U&#39;, Q contains the product X*Q;
        /// if VECT = &#39;N&#39;, the array Q is not referenced.
        /// </param>
        /// <param name="ldq">
        /// [in] LDQ is INTEGER.
        /// The leading dimension of the array Q.
        /// LDQ &gt;= 1, and LDQ &gt;= N if VECT = &#39;V&#39; or &#39;U&#39;.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        /// <remarks>
        /// <para>
        ///  Modified by Linda Kaufman, Bell Labs.
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dsbtrd", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dsbtrd(
            MatrixLayout matrixLayout,
            char vect,
            char uplo,
            int n,
            int kd,
            double* ab,
            int ldab,
            double* d,
            double* e,
            double* q,
            int ldq);
    }
}
