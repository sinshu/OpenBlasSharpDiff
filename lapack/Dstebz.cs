using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DSTEBZ computes the eigenvalues of a symmetric tridiagonal
        /// matrix T.  The user may ask for all eigenvalues, all eigenvalues
        /// in the half-open interval (VL, VU], or the IL-th through IU-th
        /// eigenvalues.
        /// </para>
        /// <para>
        /// To avoid overflow, the matrix must be scaled so that its
        /// largest element is no greater than overflow**(1/2) * underflow**(1/4) in absolute value, and for greatest
        /// accuracy, it should not be much smaller than that.
        /// </para>
        /// <para>
        /// See W. Kahan &quot;Accurate Eigenvalues of a Symmetric Tridiagonal
        /// Matrix&quot;, Report CS41, Computer Science Dept., Stanford
        /// University, July 21, 1966.
        /// </para>
        /// </summary>
        /// <param name="range">
        /// [in] RANGE is CHARACTER*1.
        /// = &#39;A&#39;: (&quot;All&quot;)   all eigenvalues will be found.
        /// = &#39;V&#39;: (&quot;Value&quot;) all eigenvalues in the half-open interval
        /// (VL, VU] will be found.
        /// = &#39;I&#39;: (&quot;Index&quot;) the IL-th through IU-th eigenvalues (of the
        /// entire matrix) will be found.
        /// </param>
        /// <param name="order">
        /// [in] ORDER is CHARACTER*1.
        /// = &#39;B&#39;: (&quot;By Block&quot;) the eigenvalues will be grouped by
        /// split-off block (see IBLOCK, ISPLIT) and
        /// ordered from smallest to largest within
        /// the block.
        /// = &#39;E&#39;: (&quot;Entire matrix&quot;)
        /// the eigenvalues for the entire matrix
        /// will be ordered from smallest to
        /// largest.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the tridiagonal matrix T.  N &gt;= 0.
        /// </param>
        /// <param name="vl">
        /// [in] VL is DOUBLE PRECISION.
        /// 
        /// If RANGE=&#39;V&#39;, the lower bound of the interval to
        /// be searched for eigenvalues.  Eigenvalues less than or equal
        /// to VL, or greater than VU, will not be returned.  VL &lt; VU.
        /// Not referenced if RANGE = &#39;A&#39; or &#39;I&#39;.
        /// </param>
        /// <param name="vu">
        /// [in] VU is DOUBLE PRECISION.
        /// 
        /// If RANGE=&#39;V&#39;, the upper bound of the interval to
        /// be searched for eigenvalues.  Eigenvalues less than or equal
        /// to VL, or greater than VU, will not be returned.  VL &lt; VU.
        /// Not referenced if RANGE = &#39;A&#39; or &#39;I&#39;.
        /// </param>
        /// <param name="il">
        /// [in] IL is INTEGER.
        /// 
        /// If RANGE=&#39;I&#39;, the index of the
        /// smallest eigenvalue to be returned.
        /// 1 &lt;= IL &lt;= IU &lt;= N, if N &gt; 0; IL = 1 and IU = 0 if N = 0.
        /// Not referenced if RANGE = &#39;A&#39; or &#39;V&#39;.
        /// </param>
        /// <param name="iu">
        /// [in] IU is INTEGER.
        /// 
        /// If RANGE=&#39;I&#39;, the index of the
        /// largest eigenvalue to be returned.
        /// 1 &lt;= IL &lt;= IU &lt;= N, if N &gt; 0; IL = 1 and IU = 0 if N = 0.
        /// Not referenced if RANGE = &#39;A&#39; or &#39;V&#39;.
        /// </param>
        /// <param name="abstol">
        /// [in] ABSTOL is DOUBLE PRECISION.
        /// The absolute tolerance for the eigenvalues.  An eigenvalue
        /// (or cluster) is considered to be located if it has been
        /// determined to lie in an interval whose width is ABSTOL or
        /// less.  If ABSTOL is less than or equal to zero, then ULP*|T|
        /// will be used, where |T| means the 1-norm of T.
        /// 
        /// Eigenvalues will be computed most accurately when ABSTOL is
        /// set to twice the underflow threshold 2*DLAMCH(&#39;S&#39;), not zero.
        /// </param>
        /// <param name="d">
        /// [in] D is DOUBLE PRECISION array, dimension (N).
        /// The n diagonal elements of the tridiagonal matrix T.
        /// </param>
        /// <param name="e">
        /// [in] E is DOUBLE PRECISION array, dimension (N-1).
        /// The (n-1) off-diagonal elements of the tridiagonal matrix T.
        /// </param>
        /// <param name="m">
        /// [out] M is INTEGER.
        /// The actual number of eigenvalues found. 0 &lt;= M &lt;= N.
        /// (See also the description of INFO=2,3.)
        /// </param>
        /// <param name="nsplit">
        /// [out] NSPLIT is INTEGER.
        /// The number of diagonal blocks in the matrix T.
        /// 1 &lt;= NSPLIT &lt;= N.
        /// </param>
        /// <param name="w">
        /// [out] W is DOUBLE PRECISION array, dimension (N).
        /// On exit, the first M elements of W will contain the
        /// eigenvalues.  (DSTEBZ may use the remaining N-M elements as
        /// workspace.)
        /// </param>
        /// <param name="iblock">
        /// [out] IBLOCK is INTEGER array, dimension (N).
        /// At each row/column j where E(j) is zero or small, the
        /// matrix T is considered to split into a block diagonal
        /// matrix.  On exit, if INFO = 0, IBLOCK(i) specifies to which
        /// block (from 1 to the number of blocks) the eigenvalue W(i)
        /// belongs.  (DSTEBZ may use the remaining N-M elements as
        /// workspace.)
        /// </param>
        /// <param name="isplit">
        /// [out] ISPLIT is INTEGER array, dimension (N).
        /// The splitting points, at which T breaks up into submatrices.
        /// The first submatrix consists of rows/columns 1 to ISPLIT(1),
        /// the second of rows/columns ISPLIT(1)+1 through ISPLIT(2),
        /// etc., and the NSPLIT-th consists of rows/columns
        /// ISPLIT(NSPLIT-1)+1 through ISPLIT(NSPLIT)=N.
        /// (Only the first NSPLIT elements will actually be used, but
        /// since the user cannot know a priori what value NSPLIT will
        /// have, N words must be reserved for ISPLIT.)
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  some or all of the eigenvalues failed to converge or
        /// were not computed:
        /// =1 or 3: Bisection failed to converge for some
        /// eigenvalues; these eigenvalues are flagged by a
        /// negative block number.  The effect is that the
        /// eigenvalues may not be as accurate as the
        /// absolute and relative tolerances.  This is
        /// generally caused by unexpectedly inaccurate
        /// arithmetic.
        /// =2 or 3: RANGE=&#39;I&#39; only: Not all of the eigenvalues
        /// IL:IU were found.
        /// Effect: M &lt; IU+1-IL
        /// Cause:  non-monotonic arithmetic, causing the
        /// Sturm sequence to be non-monotonic.
        /// Cure:   recalculate, using RANGE=&#39;A&#39;, and pick
        /// out eigenvalues IL:IU.  In some cases,
        /// increasing the PARAMETER &quot;FUDGE&quot; may
        /// make things work.
        /// = 4:    RANGE=&#39;I&#39;, and the Gershgorin interval
        /// initially used was too small.  No eigenvalues
        /// were computed.
        /// Probable cause: your machine has sloppy
        /// floating-point arithmetic.
        /// Cure: Increase the PARAMETER &quot;FUDGE&quot;,
        /// recompile, and try again.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dstebz", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dstebz(
            char range,
            char order,
            int n,
            double vl,
            double vu,
            int il,
            int iu,
            double abstol,
            double* d,
            double* e,
            int* m,
            int* nsplit,
            double* w,
            int* iblock,
            int* isplit);
    }
}
