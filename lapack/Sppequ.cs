using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// SPPEQU computes row and column scalings intended to equilibrate a
        /// symmetric positive definite matrix A in packed storage and reduce
        /// its condition number (with respect to the two-norm).  S contains the
        /// scale factors, S(i)=1/sqrt(A(i,i)), chosen so that the scaled matrix
        /// B with elements B(i,j)=S(i)*A(i,j)*S(j) has ones on the diagonal.
        /// This choice of S puts the condition number of B within a factor N of
        /// the smallest possible condition number over all possible diagonal
        /// scalings.
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
        /// [in] AP is REAL array, dimension (N*(N+1)/2).
        /// The upper or lower triangle of the symmetric matrix A, packed
        /// columnwise in a linear array.  The j-th column of A is stored
        /// in the array AP as follows:
        /// if UPLO = &#39;U&#39;, AP(i + (j-1)*j/2) = A(i,j) for 1&lt;=i&lt;=j;
        /// if UPLO = &#39;L&#39;, AP(i + (j-1)*(2n-j)/2) = A(i,j) for j&lt;=i&lt;=n.
        /// </param>
        /// <param name="s">
        /// [out] S is REAL array, dimension (N).
        /// If INFO = 0, S contains the scale factors for A.
        /// </param>
        /// <param name="scond">
        /// [out] SCOND is REAL.
        /// If INFO = 0, S contains the ratio of the smallest S(i) to
        /// the largest S(i).  If SCOND &gt;= 0.1 and AMAX is neither too
        /// large nor too small, it is not worth scaling by S.
        /// </param>
        /// <param name="amax">
        /// [out] AMAX is REAL.
        /// Absolute value of largest matrix element.  If AMAX is very
        /// close to overflow or very close to underflow, the matrix
        /// should be scaled.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  if INFO = i, the i-th diagonal element is nonpositive.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_sppequ", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Sppequ(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            float* ap,
            float* s,
            float* scond,
            float* amax);
    }
}
