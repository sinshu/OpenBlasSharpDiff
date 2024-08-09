using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CPBTRF computes the Cholesky factorization of a complex Hermitian
        /// positive definite band matrix A.
        /// </para>
        /// <para>
        /// The factorization has the form
        ///    A = U**H * U,  if UPLO = &#39;U&#39;, or
        ///    A = L  * L**H,  if UPLO = &#39;L&#39;,
        /// where U is an upper triangular matrix and L is lower triangular.
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
        /// <param name="kd">
        /// [in] KD is INTEGER.
        /// The number of superdiagonals of the matrix A if UPLO = &#39;U&#39;,
        /// or the number of subdiagonals if UPLO = &#39;L&#39;.  KD &gt;= 0.
        /// </param>
        /// <param name="ab">
        /// [in,out] AB is COMPLEX array, dimension (LDAB,N).
        /// On entry, the upper or lower triangle of the Hermitian band
        /// matrix A, stored in the first KD+1 rows of the array.  The
        /// j-th column of A is stored in the j-th column of the array AB
        /// as follows:
        /// if UPLO = &#39;U&#39;, AB(kd+1+i-j,j) = A(i,j) for max(1,j-kd)&lt;=i&lt;=j;
        /// if UPLO = &#39;L&#39;, AB(1+i-j,j)    = A(i,j) for j&lt;=i&lt;=min(n,j+kd).
        /// 
        /// On exit, if INFO = 0, the triangular factor U or L from the
        /// Cholesky factorization A = U**H*U or A = L*L**H of the band
        /// matrix A, in the same storage format as A.
        /// </param>
        /// <param name="ldab">
        /// [in] LDAB is INTEGER.
        /// The leading dimension of the array AB.  LDAB &gt;= KD+1.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  if INFO = i, the leading principal minor of order i
        /// is not positive, and the factorization could not be
        /// completed.
        /// </returns>
        /// <remarks>
        /// <para>
        ///  The band storage scheme is illustrated by the following example, when
        ///  N = 6, KD = 2, and UPLO = &#39;U&#39;:
        /// </para>
        /// <para>
        ///  On entry:                       On exit:
        /// </para>
        /// <para>
        ///      *    *   a13  a24  a35  a46      *    *   u13  u24  u35  u46
        ///      *   a12  a23  a34  a45  a56      *   u12  u23  u34  u45  u56
        ///     a11  a22  a33  a44  a55  a66     u11  u22  u33  u44  u55  u66
        /// </para>
        /// <para>
        ///  Similarly, if UPLO = &#39;L&#39; the format of A is as follows:
        /// </para>
        /// <para>
        ///  On entry:                       On exit:
        /// </para>
        /// <para>
        ///     a11  a22  a33  a44  a55  a66     l11  l22  l33  l44  l55  l66
        ///     a21  a32  a43  a54  a65   *      l21  l32  l43  l54  l65   *
        ///     a31  a42  a53  a64   *    *      l31  l42  l53  l64   *    *
        /// </para>
        /// <para>
        ///  Array elements marked * are not used by the routine.
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_cpbtrf", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Cpbtrf(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            int kd,
            Complex32* ab,
            int ldab);
    }
}
