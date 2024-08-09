using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CSPTRI computes the inverse of a complex symmetric indefinite matrix
        /// A in packed storage using the factorization A = U*D*U**T or
        /// A = L*D*L**T computed by CSPTRF.
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
        /// <param name="ap">
        /// [in,out] AP is COMPLEX array, dimension (N*(N+1)/2).
        /// On entry, the block diagonal matrix D and the multipliers
        /// used to obtain the factor U or L as computed by CSPTRF,
        /// stored as a packed triangular matrix.
        /// 
        /// On exit, if INFO = 0, the (symmetric) inverse of the original
        /// matrix, stored as a packed triangular matrix. The j-th column
        /// of inv(A) is stored in the array AP as follows:
        /// if UPLO = &#39;U&#39;, AP(i + (j-1)*j/2) = inv(A)(i,j) for 1&lt;=i&lt;=j;
        /// if UPLO = &#39;L&#39;,
        /// AP(i + (j-1)*(2n-j)/2) = inv(A)(i,j) for j&lt;=i&lt;=n.
        /// </param>
        /// <param name="ipiv">
        /// [in] IPIV is INTEGER array, dimension (N).
        /// Details of the interchanges and the block structure of D
        /// as determined by CSPTRF.
        /// </param>
        /// <returns>
        /// = 0: successful exit
        /// &lt; 0: if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0: if INFO = i, D(i,i) = 0; the matrix is singular and its
        /// inverse could not be computed.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_csptri", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Csptri(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            Complex32* ap,
            int* ipiv);
    }
}
