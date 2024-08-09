using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZPTCON computes the reciprocal of the condition number (in the
        /// 1-norm) of a complex Hermitian positive definite tridiagonal matrix
        /// using the factorization A = L*D*L**H or A = U**H*D*U computed by
        /// ZPTTRF.
        /// </para>
        /// <para>
        /// Norm(inv(A)) is computed by a direct method, and the reciprocal of
        /// the condition number is computed as
        ///                  RCOND = 1 / (ANORM * norm(inv(A))).
        /// </para>
        /// </summary>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="d">
        /// [in] D is DOUBLE PRECISION array, dimension (N).
        /// The n diagonal elements of the diagonal matrix D from the
        /// factorization of A, as computed by ZPTTRF.
        /// </param>
        /// <param name="e">
        /// [in] E is COMPLEX*16 array, dimension (N-1).
        /// The (n-1) off-diagonal elements of the unit bidiagonal factor
        /// U or L from the factorization of A, as computed by ZPTTRF.
        /// </param>
        /// <param name="anorm">
        /// [in] ANORM is DOUBLE PRECISION.
        /// The 1-norm of the original matrix A.
        /// </param>
        /// <param name="rcond">
        /// [out] RCOND is DOUBLE PRECISION.
        /// The reciprocal of the condition number of the matrix A,
        /// computed as RCOND = 1/(ANORM * AINVNM), where AINVNM is the
        /// 1-norm of inv(A) computed in this routine.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        /// <remarks>
        /// <para>
        ///  The method used is described in Nicholas J. Higham, &quot;Efficient
        ///  Algorithms for Computing the Condition Number of a Tridiagonal
        ///  Matrix&quot;, SIAM J. Sci. Stat. Comput., Vol. 7, No. 1, January 1986.
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zptcon", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zptcon(
            int n,
            double* d,
            Complex* e,
            double anorm,
            double* rcond);
    }
}
