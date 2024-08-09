using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZHESWAPR applies an elementary permutation on the rows and the columns of
        /// a hermitian matrix.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// Specifies whether the details of the factorization are stored
        /// as an upper or lower triangular matrix.
        /// = &#39;U&#39;:  Upper triangular, form is A = U*D*U**T;
        /// = &#39;L&#39;:  Lower triangular, form is A = L*D*L**T.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is COMPLEX*16 array, dimension (LDA,N).
        /// On entry, the NB diagonal matrix D and the multipliers
        /// used to obtain the factor U or L as computed by CSYTRF.
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
        /// <param name="i1">
        /// [in] I1 is INTEGER.
        /// Index of the first row to swap
        /// </param>
        /// <param name="i2">
        /// [in] I2 is INTEGER.
        /// Index of the second row to swap
        /// </param>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zheswapr", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zheswapr(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            Complex* a,
            int lda,
            int i1,
            int i2);
    }
}
