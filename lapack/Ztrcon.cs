using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZTRCON estimates the reciprocal of the condition number of a
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
        /// <param name="a">
        /// [in] A is COMPLEX*16 array, dimension (LDA,N).
        /// The triangular matrix A.  If UPLO = &#39;U&#39;, the leading N-by-N
        /// upper triangular part of the array A contains the upper
        /// triangular matrix, and the strictly lower triangular part of
        /// A is not referenced.  If UPLO = &#39;L&#39;, the leading N-by-N lower
        /// triangular part of the array A contains the lower triangular
        /// matrix, and the strictly upper triangular part of A is not
        /// referenced.  If DIAG = &#39;U&#39;, the diagonal elements of A are
        /// also not referenced and are assumed to be 1.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
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
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_ztrcon", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Ztrcon(
            MatrixLayout matrixLayout,
            char norm,
            char uplo,
            char diag,
            int n,
            Complex* a,
            int lda,
            double* rcond);
    }
}
