using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZTPCON estimates the reciprocal of the condition number of a packed
        /// triangular matrix A, in either the 1-norm or the infinity-norm.
        /// </para>
        /// <para>
        /// The norm of A is computed and an estimate is obtained for
        /// norm(inv(A)), then the reciprocal of the condition number is
        /// computed as
        ///    RCOND = 1 / ( norm(A) * norm(inv(A)) ).
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="norm">
        /// [in] NORM is CHARACTER*1.
        /// Specifies whether the 1-norm condition number or the
        /// infinity-norm condition number is required:
        /// = &#39;1&#39; or &#39;O&#39;:  1-norm;
        /// = &#39;I&#39;:         Infinity-norm.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// = &#39;U&#39;:  A is upper triangular;
        /// = &#39;L&#39;:  A is lower triangular.
        /// </param>
        /// <param name="diag">
        /// [in] DIAG is CHARACTER*1.
        /// = &#39;N&#39;:  A is non-unit triangular;
        /// = &#39;U&#39;:  A is unit triangular.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="ap">
        /// [in] AP is COMPLEX*16 array, dimension (N*(N+1)/2).
        /// The upper or lower triangular matrix A, packed columnwise in
        /// a linear array.  The j-th column of A is stored in the array
        /// AP as follows:
        /// if UPLO = &#39;U&#39;, AP(i + (j-1)*j/2) = A(i,j) for 1&lt;=i&lt;=j;
        /// if UPLO = &#39;L&#39;, AP(i + (j-1)*(2n-j)/2) = A(i,j) for j&lt;=i&lt;=n.
        /// If DIAG = &#39;U&#39;, the diagonal elements of A are not referenced
        /// and are assumed to be 1.
        /// </param>
        /// <param name="rcond">
        /// [out] RCOND is DOUBLE PRECISION.
        /// The reciprocal of the condition number of the matrix A,
        /// computed as RCOND = 1/(norm(A) * norm(inv(A))).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_ztpcon", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Ztpcon(
            MatrixLayout matrixLayout,
            char norm,
            char uplo,
            char diag,
            int n,
            Complex* ap,
            double* rcond);
    }
}
