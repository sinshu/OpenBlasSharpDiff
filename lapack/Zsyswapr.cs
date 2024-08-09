using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZSYSWAPR applies an elementary permutation on the rows and the columns of
        /// a symmetric matrix.
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
        /// [in,out] A is COMPLEX*16 array, dimension (LDA,*).
        /// On entry, the N-by-N matrix A. On exit, the permuted matrix
        /// where the rows I1 and I2 and columns I1 and I2 are interchanged.
        /// If UPLO = &#39;U&#39;, the interchanges are applied to the upper
        /// triangular part and the strictly lower triangular part of A is
        /// not referenced; if UPLO = &#39;L&#39;, the interchanges are applied to
        /// the lower triangular part and the part of A above the diagonal
        /// is not referenced.
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
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zsyswapr", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zsyswapr(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            Complex* a,
            int lda,
            int i1,
            int i2);
    }
}
