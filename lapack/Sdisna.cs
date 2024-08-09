using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// SDISNA computes the reciprocal condition numbers for the eigenvectors
        /// of a real symmetric or complex Hermitian matrix or for the left or
        /// right singular vectors of a general m-by-n matrix. The reciprocal
        /// condition number is the &#39;gap&#39; between the corresponding eigenvalue or
        /// singular value and the nearest other one.
        /// </para>
        /// <para>
        /// The bound on the error, measured by angle in radians, in the I-th
        /// computed vector is given by
        /// </para>
        /// <para>
        ///        SLAMCH( &#39;E&#39; ) * ( ANORM / SEP( I ) )
        /// </para>
        /// <para>
        /// where ANORM = 2-norm(A) = max( abs( D(j) ) ).  SEP(I) is not allowed
        /// to be smaller than SLAMCH( &#39;E&#39; )*ANORM in order to limit the size of
        /// the error bound.
        /// </para>
        /// <para>
        /// SDISNA may also be used to compute error bounds for eigenvectors of
        /// the generalized symmetric definite eigenproblem.
        /// </para>
        /// </summary>
        /// <param name="job">
        /// [in] JOB is CHARACTER*1.
        /// Specifies for which problem the reciprocal condition numbers
        /// should be computed:
        /// = &#39;E&#39;:  the eigenvectors of a symmetric/Hermitian matrix;
        /// = &#39;L&#39;:  the left singular vectors of a general matrix;
        /// = &#39;R&#39;:  the right singular vectors of a general matrix.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of rows of the matrix. M &gt;= 0.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// If JOB = &#39;L&#39; or &#39;R&#39;, the number of columns of the matrix,
        /// in which case N &gt;= 0. Ignored if JOB = &#39;E&#39;.
        /// </param>
        /// <param name="d">
        /// [in] D is REAL array, dimension (M) if JOB = &#39;E&#39;.
        /// dimension (min(M,N)) if JOB = &#39;L&#39; or &#39;R&#39;
        /// The eigenvalues (if JOB = &#39;E&#39;) or singular values (if JOB =
        /// &#39;L&#39; or &#39;R&#39;) of the matrix, in either increasing or decreasing
        /// order. If singular values, they must be non-negative.
        /// </param>
        /// <param name="sep">
        /// [out] SEP is REAL array, dimension (M) if JOB = &#39;E&#39;.
        /// dimension (min(M,N)) if JOB = &#39;L&#39; or &#39;R&#39;
        /// The reciprocal condition numbers of the vectors.
        /// </param>
        /// <returns>
        /// = 0:  successful exit.
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_sdisna", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Sdisna(
            char job,
            int m,
            int n,
            float* d,
            float* sep);
    }
}
