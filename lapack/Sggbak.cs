using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// SGGBAK forms the right or left eigenvectors of a real generalized
        /// eigenvalue problem A*x = lambda*B*x, by backward transformation on
        /// the computed eigenvectors of the balanced pair of matrices output by
        /// SGGBAL.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="job">
        /// [in] JOB is CHARACTER*1.
        /// Specifies the type of backward transformation required:
        /// = &#39;N&#39;:  do nothing, return immediately;
        /// = &#39;P&#39;:  do backward transformation for permutation only;
        /// = &#39;S&#39;:  do backward transformation for scaling only;
        /// = &#39;B&#39;:  do backward transformations for both permutation and
        /// scaling.
        /// JOB must be the same as the argument JOB supplied to SGGBAL.
        /// </param>
        /// <param name="side">
        /// [in] SIDE is CHARACTER*1.
        /// = &#39;R&#39;:  V contains right eigenvectors;
        /// = &#39;L&#39;:  V contains left eigenvectors.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of rows of the matrix V.  N &gt;= 0.
        /// </param>
        /// <param name="ilo">
        /// [in] ILO is INTEGER.
        /// </param>
        /// <param name="ihi">
        /// [in] IHI is INTEGER.
        /// The integers ILO and IHI determined by SGGBAL.
        /// 1 &lt;= ILO &lt;= IHI &lt;= N, if N &gt; 0; ILO=1 and IHI=0, if N=0.
        /// </param>
        /// <param name="lscale">
        /// [in] LSCALE is REAL array, dimension (N).
        /// Details of the permutations and/or scaling factors applied
        /// to the left side of A and B, as returned by SGGBAL.
        /// </param>
        /// <param name="rscale">
        /// [in] RSCALE is REAL array, dimension (N).
        /// Details of the permutations and/or scaling factors applied
        /// to the right side of A and B, as returned by SGGBAL.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of columns of the matrix V.  M &gt;= 0.
        /// </param>
        /// <param name="v">
        /// [in,out] V is REAL array, dimension (LDV,M).
        /// On entry, the matrix of right or left eigenvectors to be
        /// transformed, as returned by STGEVC.
        /// On exit, V is overwritten by the transformed eigenvectors.
        /// </param>
        /// <param name="ldv">
        /// [in] LDV is INTEGER.
        /// The leading dimension of the matrix V. LDV &gt;= max(1,N).
        /// </param>
        /// <returns>
        /// = 0:  successful exit.
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value.
        /// </returns>
        /// <remarks>
        /// <para>
        ///  See R.C. Ward, Balancing the generalized eigenvalue problem,
        ///                 SIAM J. Sci. Stat. Comp. 2 (1981), 141-152.
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_sggbak", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Sggbak(
            MatrixLayout matrixLayout,
            char job,
            char side,
            int n,
            int ilo,
            int ihi,
            float* lscale,
            float* rscale,
            int m,
            float* v,
            int ldv);
    }
}
