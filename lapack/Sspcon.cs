using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// SSPCON estimates the reciprocal of the condition number (in the
        /// 1-norm) of a real symmetric packed matrix A using the factorization
        /// A = U*D*U**T or A = L*D*L**T computed by SSPTRF.
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
        /// [in] AP is REAL array, dimension (N*(N+1)/2).
        /// The block diagonal matrix D and the multipliers used to
        /// obtain the factor U or L as computed by SSPTRF, stored as a
        /// packed triangular matrix.
        /// </param>
        /// <param name="ipiv">
        /// [in] IPIV is INTEGER array, dimension (N).
        /// Details of the interchanges and the block structure of D
        /// as determined by SSPTRF.
        /// </param>
        /// <param name="anorm">
        /// [in] ANORM is REAL.
        /// The 1-norm of the original matrix A.
        /// </param>
        /// <param name="rcond">
        /// [out] RCOND is REAL.
        /// The reciprocal of the condition number of the matrix A,
        /// computed as RCOND = 1/(ANORM * AINVNM), where AINVNM is an
        /// estimate of the 1-norm of inv(A) computed in this routine.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_sspcon", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Sspcon(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            float* ap,
            int* ipiv,
            float anorm,
            float* rcond);
    }
}
