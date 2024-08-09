using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// SGTCON estimates the reciprocal of the condition number of a real
        /// tridiagonal matrix A using the LU factorization as computed by
        /// SGTTRF.
        /// </para>
        /// <para>
        /// An estimate is obtained for norm(inv(A)), and the reciprocal of the
        /// condition number is computed as RCOND = 1 / (ANORM * norm(inv(A))).
        /// </para>
        /// </summary>
        /// <param name="norm">
        /// [in] NORM is CHARACTER*1.
        /// Specifies whether the 1-norm condition number or the
        /// infinity-norm condition number is required:
        /// = &#39;1&#39; or &#39;O&#39;:  1-norm;
        /// = &#39;I&#39;:         Infinity-norm.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="dl">
        /// [in] DL is REAL array, dimension (N-1).
        /// The (n-1) multipliers that define the matrix L from the
        /// LU factorization of A as computed by SGTTRF.
        /// </param>
        /// <param name="d">
        /// [in] D is REAL array, dimension (N).
        /// The n diagonal elements of the upper triangular matrix U from
        /// the LU factorization of A.
        /// </param>
        /// <param name="du">
        /// [in] DU is REAL array, dimension (N-1).
        /// The (n-1) elements of the first superdiagonal of U.
        /// </param>
        /// <param name="du2">
        /// [in] DU2 is REAL array, dimension (N-2).
        /// The (n-2) elements of the second superdiagonal of U.
        /// </param>
        /// <param name="ipiv">
        /// [in] IPIV is INTEGER array, dimension (N).
        /// The pivot indices; for 1 &lt;= i &lt;= n, row i of the matrix was
        /// interchanged with row IPIV(i).  IPIV(i) will always be either
        /// i or i+1; IPIV(i) = i indicates a row interchange was not
        /// required.
        /// </param>
        /// <param name="anorm">
        /// [in] ANORM is REAL.
        /// If NORM = &#39;1&#39; or &#39;O&#39;, the 1-norm of the original matrix A.
        /// If NORM = &#39;I&#39;, the infinity-norm of the original matrix A.
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
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_sgtcon", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Sgtcon(
            char norm,
            int n,
            float* dl,
            float* d,
            float* du,
            float* du2,
            int* ipiv,
            float anorm,
            float* rcond);
    }
}
