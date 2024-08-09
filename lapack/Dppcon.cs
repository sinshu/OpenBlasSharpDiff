using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DPPCON estimates the reciprocal of the condition number (in the
        /// 1-norm) of a real symmetric positive definite packed matrix using
        /// the Cholesky factorization A = U**T*U or A = L*L**T computed by
        /// DPPTRF.
        /// </para>
        /// <para>
        /// An estimate is obtained for norm(inv(A)), and the reciprocal of the
        /// condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
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
        /// [in] AP is DOUBLE PRECISION array, dimension (N*(N+1)/2).
        /// The triangular factor U or L from the Cholesky factorization
        /// A = U**T*U or A = L*L**T, packed columnwise in a linear
        /// array.  The j-th column of U or L is stored in the array AP
        /// as follows:
        /// if UPLO = &#39;U&#39;, AP(i + (j-1)*j/2) = U(i,j) for 1&lt;=i&lt;=j;
        /// if UPLO = &#39;L&#39;, AP(i + (j-1)*(2n-j)/2) = L(i,j) for j&lt;=i&lt;=n.
        /// </param>
        /// <param name="anorm">
        /// [in] ANORM is DOUBLE PRECISION.
        /// The 1-norm (or infinity-norm) of the symmetric matrix A.
        /// </param>
        /// <param name="rcond">
        /// [out] RCOND is DOUBLE PRECISION.
        /// The reciprocal of the condition number of the matrix A,
        /// computed as RCOND = 1/(ANORM * AINVNM), where AINVNM is an
        /// estimate of the 1-norm of inv(A) computed in this routine.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dppcon", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dppcon(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            double* ap,
            double anorm,
            double* rcond);
    }
}
