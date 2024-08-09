using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZSYCONV converts A given by ZHETRF into L and D or vice-versa.
        /// Get nondiagonal elements of D (returned in workspace) and
        /// apply or reverse permutation done in TRF.
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
        /// <param name="way">
        /// [in] WAY is CHARACTER*1.
        /// = &#39;C&#39;: Convert
        /// = &#39;R&#39;: Revert
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is COMPLEX*16 array, dimension (LDA,N).
        /// The block diagonal matrix D and the multipliers used to
        /// obtain the factor U or L as computed by ZSYTRF.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="ipiv">
        /// [in] IPIV is INTEGER array, dimension (N).
        /// Details of the interchanges and the block structure of D
        /// as determined by ZSYTRF.
        /// </param>
        /// <param name="e">
        /// [out] E is COMPLEX*16 array, dimension (N).
        /// E stores the supdiagonal/subdiagonal of the symmetric 1-by-1
        /// or 2-by-2 block diagonal matrix D in LDLT.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zsyconv", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zsyconv(
            MatrixLayout matrixLayout,
            char uplo,
            char way,
            int n,
            Complex* a,
            int lda,
            int* ipiv,
            Complex* e);
    }
}
