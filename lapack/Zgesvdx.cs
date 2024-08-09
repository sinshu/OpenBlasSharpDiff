using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        ///  ZGESVDX computes the singular value decomposition (SVD) of a complex
        ///  M-by-N matrix A, optionally computing the left and/or right singular
        ///  vectors. The SVD is written
        /// </para>
        /// <para>
        ///      A = U * SIGMA * transpose(V)
        /// </para>
        /// <para>
        ///  where SIGMA is an M-by-N matrix which is zero except for its
        ///  min(m,n) diagonal elements, U is an M-by-M unitary matrix, and
        ///  V is an N-by-N unitary matrix.  The diagonal elements of SIGMA
        ///  are the singular values of A; they are real and non-negative, and
        ///  are returned in descending order.  The first min(m,n) columns of
        ///  U and V are the left and right singular vectors of A.
        /// </para>
        /// <para>
        ///  ZGESVDX uses an eigenvalue problem for obtaining the SVD, which
        ///  allows for the computation of a subset of singular values and
        ///  vectors. See DBDSVDX for details.
        /// </para>
        /// <para>
        ///  Note that the routine returns V**T, not V.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="jobu">
        /// [in] JOBU is CHARACTER*1.
        /// Specifies options for computing all or part of the matrix U:
        /// = &#39;V&#39;:  the first min(m,n) columns of U (the left singular
        /// vectors) or as specified by RANGE are returned in
        /// the array U;
        /// = &#39;N&#39;:  no columns of U (no left singular vectors) are
        /// computed.
        /// </param>
        /// <param name="jobvt">
        /// [in] JOBVT is CHARACTER*1.
        /// Specifies options for computing all or part of the matrix
        /// V**T:
        /// = &#39;V&#39;:  the first min(m,n) rows of V**T (the right singular
        /// vectors) or as specified by RANGE are returned in
        /// the array VT;
        /// = &#39;N&#39;:  no rows of V**T (no right singular vectors) are
        /// computed.
        /// </param>
        /// <param name="range">
        /// [in] RANGE is CHARACTER*1.
        /// = &#39;A&#39;: all singular values will be found.
        /// = &#39;V&#39;: all singular values in the half-open interval (VL,VU]
        /// will be found.
        /// = &#39;I&#39;: the IL-th through IU-th singular values will be found.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of rows of the input matrix A.  M &gt;= 0.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of columns of the input matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is COMPLEX*16 array, dimension (LDA,N).
        /// On entry, the M-by-N matrix A.
        /// On exit, the contents of A are destroyed.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,M).
        /// </param>
        /// <param name="vl">
        /// [in] VL is DOUBLE PRECISION.
        /// If RANGE=&#39;V&#39;, the lower bound of the interval to
        /// be searched for singular values. VU &gt; VL.
        /// Not referenced if RANGE = &#39;A&#39; or &#39;I&#39;.
        /// </param>
        /// <param name="vu">
        /// [in] VU is DOUBLE PRECISION.
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
        /// The total number of singular values found,
        /// 0 &lt;= NS &lt;= min(M,N).
        /// If RANGE = &#39;A&#39;, NS = min(M,N); if RANGE = &#39;I&#39;, NS = IU-IL+1.
        /// </param>
        /// <param name="s">
        /// [out] S is DOUBLE PRECISION array, dimension (min(M,N)).
        /// The singular values of A, sorted so that S(i) &gt;= S(i+1).
        /// </param>
        /// <param name="u">
        /// [out] U is COMPLEX*16 array, dimension (LDU,UCOL).
        /// If JOBU = &#39;V&#39;, U contains columns of U (the left singular
        /// vectors, stored columnwise) as specified by RANGE; if
        /// JOBU = &#39;N&#39;, U is not referenced.
        /// Note: The user must ensure that UCOL &gt;= NS; if RANGE = &#39;V&#39;,
        /// the exact value of NS is not known in advance and an upper
        /// bound must be used.
        /// </param>
        /// <param name="ldu">
        /// [in] LDU is INTEGER.
        /// The leading dimension of the array U.  LDU &gt;= 1; if
        /// JOBU = &#39;V&#39;, LDU &gt;= M.
        /// </param>
        /// <param name="vt">
        /// [out] VT is COMPLEX*16 array, dimension (LDVT,N).
        /// If JOBVT = &#39;V&#39;, VT contains the rows of V**T (the right singular
        /// vectors, stored rowwise) as specified by RANGE; if JOBVT = &#39;N&#39;,
        /// VT is not referenced.
        /// Note: The user must ensure that LDVT &gt;= NS; if RANGE = &#39;V&#39;,
        /// the exact value of NS is not known in advance and an upper
        /// bound must be used.
        /// </param>
        /// <param name="ldvt">
        /// [in] LDVT is INTEGER.
        /// The leading dimension of the array VT.  LDVT &gt;= 1; if
        /// JOBVT = &#39;V&#39;, LDVT &gt;= NS (see above).
        /// </param>
        /// <param name="superb">
        /// No description available.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  if INFO = i, then i eigenvectors failed to converge
        /// in DBDSVDX/DSTEVX.
        /// if INFO = N*2 + 1, an internal error occurred in
        /// DBDSVDX
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zgesvdx", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zgesvdx(
            MatrixLayout matrixLayout,
            char jobu,
            char jobvt,
            char range,
            int m,
            int n,
            Complex* a,
            int lda,
            double vl,
            double vu,
            int il,
            int iu,
            int* ns,
            double* s,
            Complex* u,
            int ldu,
            Complex* vt,
            int ldvt,
            int* superb);
    }
}
