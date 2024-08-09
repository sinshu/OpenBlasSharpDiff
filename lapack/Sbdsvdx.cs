using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        ///  SBDSVDX computes the singular value decomposition (SVD) of a real
        ///  N-by-N (upper or lower) bidiagonal matrix B, B = U * S * VT,
        ///  where S is a diagonal matrix with non-negative diagonal elements
        ///  (the singular values of B), and U and VT are orthogonal matrices
        ///  of left and right singular vectors, respectively.
        /// </para>
        /// <para>
        ///  Given an upper bidiagonal B with diagonal D = [ d_1 d_2 ... d_N ]
        ///  and superdiagonal E = [ e_1 e_2 ... e_N-1 ], SBDSVDX computes the
        ///  singular value decomposition of B through the eigenvalues and
        ///  eigenvectors of the N*2-by-N*2 tridiagonal matrix
        /// </para>
        /// <para>
        ///        |  0  d_1                |
        ///        | d_1  0  e_1            |
        ///  TGK = |     e_1  0  d_2        |
        ///        |         d_2  .   .     |
        ///        |              .   .   . |
        /// </para>
        /// <para>
        ///  If (s,u,v) is a singular triplet of B with ||u|| = ||v|| = 1, then
        ///  (+/-s,q), ||q|| = 1, are eigenpairs of TGK, with q = P * ( u&#39; +/-v&#39; ) /
        ///  sqrt(2) = ( v_1 u_1 v_2 u_2 ... v_n u_n ) / sqrt(2), and
        ///  P = [ e_{n+1} e_{1} e_{n+2} e_{2} ... ].
        /// </para>
        /// <para>
        ///  Given a TGK matrix, one can either a) compute -s,-v and change signs
        ///  so that the singular values (and corresponding vectors) are already in
        ///  descending order (as in SGESVD/SGESDD) or b) compute s,v and reorder
        ///  the values (and corresponding vectors). SBDSVDX implements a) by
        ///  calling SSTEVX (bisection plus inverse iteration, to be replaced
        ///  with a version of the Multiple Relative Robust Representation
        ///  algorithm. (See P. Willems and B. Lang, A framework for the MR^3
        ///  algorithm: theory and implementation, SIAM J. Sci. Comput.,
        ///  35:740-766, 2013.)
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// = &#39;U&#39;:  B is upper bidiagonal;
        /// = &#39;L&#39;:  B is lower bidiagonal.
        /// </param>
        /// <param name="jobz">
        /// [in] JOBZ is CHARACTER*1.
        /// = &#39;N&#39;:  Compute singular values only;
        /// = &#39;V&#39;:  Compute singular values and singular vectors.
        /// </param>
        /// <param name="range">
        /// [in] RANGE is CHARACTER*1.
        /// = &#39;A&#39;: all singular values will be found.
        /// = &#39;V&#39;: all singular values in the half-open interval [VL,VU)
        /// will be found.
        /// = &#39;I&#39;: the IL-th through IU-th singular values will be found.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the bidiagonal matrix.  N &gt;= 0.
        /// </param>
        /// <param name="d">
        /// [in] D is REAL array, dimension (N).
        /// The n diagonal elements of the bidiagonal matrix B.
        /// </param>
        /// <param name="e">
        /// [in] E is REAL array, dimension (max(1,N-1)).
        /// The (n-1) superdiagonal elements of the bidiagonal matrix
        /// B in elements 1 to N-1.
        /// </param>
        /// <param name="vl">
        /// [in] VL is REAL.
        /// If RANGE=&#39;V&#39;, the lower bound of the interval to
        /// be searched for singular values. VU &gt; VL.
        /// Not referenced if RANGE = &#39;A&#39; or &#39;I&#39;.
        /// </param>
        /// <param name="vu">
        /// [in] VU is REAL.
        /// If RANGE=&#39;V&#39;, the upper bound of the interval to
        /// be searched for singular values. VU &gt; VL.
        /// Not referenced if RANGE = &#39;A&#39; or &#39;I&#39;.
        /// </param>
        /// <param name="il">
        /// [in] IL is INTEGER.
        /// If RANGE=&#39;I&#39;, the index of the
        /// smallest singular value to be returned.
        /// 1 &lt;= IL &lt;= IU &lt;= min(M,N), if min(M,N) &gt; 0.
        /// Not referenced if RANGE = &#39;A&#39; or &#39;V&#39;.
        /// </param>
        /// <param name="iu">
        /// [in] IU is INTEGER.
        /// If RANGE=&#39;I&#39;, the index of the
        /// largest singular value to be returned.
        /// 1 &lt;= IL &lt;= IU &lt;= min(M,N), if min(M,N) &gt; 0.
        /// Not referenced if RANGE = &#39;A&#39; or &#39;V&#39;.
        /// </param>
        /// <param name="ns">
        /// [out] NS is INTEGER.
        /// The total number of singular values found.  0 &lt;= NS &lt;= N.
        /// If RANGE = &#39;A&#39;, NS = N, and if RANGE = &#39;I&#39;, NS = IU-IL+1.
        /// </param>
        /// <param name="s">
        /// [out] S is REAL array, dimension (N).
        /// The first NS elements contain the selected singular values in
        /// ascending order.
        /// </param>
        /// <param name="z">
        /// [out] Z is REAL array, dimension (2*N,K).
        /// If JOBZ = &#39;V&#39;, then if INFO = 0 the first NS columns of Z
        /// contain the singular vectors of the matrix B corresponding to
        /// the selected singular values, with U in rows 1 to N and V
        /// in rows N+1 to N*2, i.e.
        /// Z = [ U ]
        /// [ V ]
        /// If JOBZ = &#39;N&#39;, then Z is not referenced.
        /// Note: The user must ensure that at least K = NS+1 columns are
        /// supplied in the array Z; if RANGE = &#39;V&#39;, the exact value of
        /// NS is not known in advance and an upper bound must be used.
        /// </param>
        /// <param name="ldz">
        /// [in] LDZ is INTEGER.
        /// The leading dimension of the array Z. LDZ &gt;= 1, and if
        /// JOBZ = &#39;V&#39;, LDZ &gt;= max(2,N*2).
        /// </param>
        /// <param name="superb">
        /// No description available.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  if INFO = i, then i eigenvectors failed to converge
        /// in SSTEVX. The indices of the eigenvectors
        /// (as returned by SSTEVX) are stored in the
        /// array IWORK.
        /// if INFO = N*2 + 1, an internal error occurred.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_sbdsvdx", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Sbdsvdx(
            MatrixLayout matrixLayout,
            char uplo,
            char jobz,
            char range,
            int n,
            float* d,
            float* e,
            float vl,
            float vu,
            int il,
            int iu,
            int* ns,
            float* s,
            float* z,
            int ldz,
            int* superb);
    }
}
