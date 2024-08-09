using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CSYTRF_AA computes the factorization of a complex symmetric matrix A
        /// using the Aasen&#39;s algorithm.  The form of the factorization is
        /// </para>
        /// <para>
        ///    A = U**T*T*U  or  A = L*T*L**T
        /// </para>
        /// <para>
        /// where U (or L) is a product of permutation and unit upper (lower)
        /// triangular matrices, and T is a complex symmetric tridiagonal matrix.
        /// </para>
        /// <para>
        /// This is the blocked version of the algorithm, calling Level 3 BLAS.
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
        /// [in,out] A is COMPLEX array, dimension (LDA,N).
        /// On entry, the symmetric matrix A.  If UPLO = &#39;U&#39;, the leading
        /// N-by-N upper triangular part of A contains the upper
        /// triangular part of the matrix A, and the strictly lower
        /// triangular part of A is not referenced.  If UPLO = &#39;L&#39;, the
        /// leading N-by-N lower triangular part of A contains the lower
        /// triangular part of the matrix A, and the strictly upper
        /// triangular part of A is not referenced.
        /// 
        /// On exit, the tridiagonal matrix is stored in the diagonals
        /// and the subdiagonals of A just below (or above) the diagonals,
        /// and L is stored below (or above) the subdiagonals, when UPLO
        /// is &#39;L&#39; (or &#39;U&#39;).
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="ipiv">
        /// [out] IPIV is INTEGER array, dimension (N).
        /// On exit, it contains the details of the interchanges, i.e.,
        /// the row and column k of A were interchanged with the
        /// row and column IPIV(k).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_csytrf_aa", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo CsytrfAa(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            Complex32* a,
            int lda,
            int* ipiv);
    }
}
