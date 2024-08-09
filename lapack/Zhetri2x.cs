using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZHETRI2X computes the inverse of a COMPLEX*16 Hermitian indefinite matrix
        /// A using the factorization A = U*D*U**H or A = L*D*L**H computed by
        /// ZHETRF.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// Specifies whether the details of the factorization are stored
        /// as an upper or lower triangular matrix.
        /// = &#39;U&#39;:  Upper triangular, form is A = U*D*U**H;
        /// = &#39;L&#39;:  Lower triangular, form is A = L*D*L**H.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is COMPLEX*16 array, dimension (LDA,N).
        /// On entry, the NNB diagonal matrix D and the multipliers
        /// used to obtain the factor U or L as computed by ZHETRF.
        /// 
        /// On exit, if INFO = 0, the (symmetric) inverse of the original
        /// matrix.  If UPLO = &#39;U&#39;, the upper triangular part of the
        /// inverse is formed and the part of A below the diagonal is not
        /// referenced; if UPLO = &#39;L&#39; the lower triangular part of the
        /// inverse is formed and the part of A above the diagonal is
        /// not referenced.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="ipiv">
        /// [in] IPIV is INTEGER array, dimension (N).
        /// Details of the interchanges and the NNB structure of D
        /// as determined by ZHETRF.
        /// </param>
        /// <param name="nb">
        /// [in] NB is INTEGER.
        /// Block size
        /// </param>
        /// <returns>
        /// = 0: successful exit
        /// &lt; 0: if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0: if INFO = i, D(i,i) = 0; the matrix is singular and its
        /// inverse could not be computed.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zhetri2x", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zhetri2x(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            Complex* a,
            int lda,
            int* ipiv,
            int nb);
    }
}
