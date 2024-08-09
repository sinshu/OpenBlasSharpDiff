using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// SGESVDQ computes the singular value decomposition (SVD) of a real
        /// M-by-N matrix A, where M &gt;= N. The SVD of A is written as
        ///                                    [++]   [xx]   [x0]   [xx]
        ///              A = U * SIGMA * V^*,  [++] = [xx] * [ox] * [xx]
        ///                                    [++]   [xx]
        /// where SIGMA is an N-by-N diagonal matrix, U is an M-by-N orthonormal
        /// matrix, and V is an N-by-N orthogonal matrix. The diagonal elements
        /// of SIGMA are the singular values of A. The columns of U and V are the
        /// left and the right singular vectors of A, respectively.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="joba">
        /// [in] JOBA is CHARACTER*1.
        /// Specifies the level of accuracy in the computed SVD
        /// = &#39;A&#39; The requested accuracy corresponds to having the backward
        /// error bounded by || delta A ||_F &lt;= f(m,n) * EPS * || A ||_F,
        /// where EPS = SLAMCH(&#39;Epsilon&#39;). This authorises CGESVDQ to
        /// truncate the computed triangular factor in a rank revealing
        /// QR factorization whenever the truncated part is below the
        /// threshold of the order of EPS * ||A||_F. This is aggressive
        /// truncation level.
        /// = &#39;M&#39; Similarly as with &#39;A&#39;, but the truncation is more gentle: it
        /// is allowed only when there is a drop on the diagonal of the
        /// triangular factor in the QR factorization. This is medium
        /// truncation level.
        /// = &#39;H&#39; High accuracy requested. No numerical rank determination based
        /// on the rank revealing QR factorization is attempted.
        /// = &#39;E&#39; Same as &#39;H&#39;, and in addition the condition number of column
        /// scaled A is estimated and returned in  RWORK(1).
        /// N^(-1/4)*RWORK(1) &lt;= ||pinv(A_scaled)||_2 &lt;= N^(1/4)*RWORK(1)
        /// </param>
        /// <param name="jobp">
        /// [in] JOBP is CHARACTER*1.
        /// = &#39;P&#39; The rows of A are ordered in decreasing order with respect to
        /// ||A(i,:)||_\infty. This enhances numerical accuracy at the cost
        /// of extra data movement. Recommended for numerical robustness.
        /// = &#39;N&#39; No row pivoting.
        /// </param>
        /// <param name="jobr">
        /// [in] JOBR is CHARACTER*1.
        /// = &#39;T&#39; After the initial pivoted QR factorization, SGESVD is applied to
        /// the transposed R**T of the computed triangular factor R. This involves
        /// some extra data movement (matrix transpositions). Useful for
        /// experiments, research and development.
        /// = &#39;N&#39; The triangular factor R is given as input to SGESVD. This may be
        /// preferred as it involves less data movement.
        /// </param>
        /// <param name="jobu">
        /// [in] JOBU is CHARACTER*1.
        /// = &#39;A&#39; All M left singular vectors are computed and returned in the
        /// matrix U. See the description of U.
        /// = &#39;S&#39; or &#39;U&#39; N = min(M,N) left singular vectors are computed and returned
        /// in the matrix U. See the description of U.
        /// = &#39;R&#39; Numerical rank NUMRANK is determined and only NUMRANK left singular
        /// vectors are computed and returned in the matrix U.
        /// = &#39;F&#39; The N left singular vectors are returned in factored form as the
        /// product of the Q factor from the initial QR factorization and the
        /// N left singular vectors of (R**T , 0)**T. If row pivoting is used,
        /// then the necessary information on the row pivoting is stored in
        /// IWORK(N+1:N+M-1).
        /// = &#39;N&#39; The left singular vectors are not computed.
        /// </param>
        /// <param name="jobv">
        /// [in] JOBV is CHARACTER*1.
        /// = &#39;A&#39;, &#39;V&#39; All N right singular vectors are computed and returned in
        /// the matrix V.
        /// = &#39;R&#39; Numerical rank NUMRANK is determined and only NUMRANK right singular
        /// vectors are computed and returned in the matrix V. This option is
        /// allowed only if JOBU = &#39;R&#39; or JOBU = &#39;N&#39;; otherwise it is illegal.
        /// = &#39;N&#39; The right singular vectors are not computed.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of rows of the input matrix A.  M &gt;= 0.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of columns of the input matrix A.  M &gt;= N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is REAL array of dimensions LDA x N.
        /// On entry, the input matrix A.
        /// On exit, if JOBU .NE. &#39;N&#39; or JOBV .NE. &#39;N&#39;, the lower triangle of A contains
        /// the Householder vectors as stored by SGEQP3. If JOBU = &#39;F&#39;, these Householder
        /// vectors together with WORK(1:N) can be used to restore the Q factors from
        /// the initial pivoted QR factorization of A. See the description of U.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,M).
        /// </param>
        /// <param name="s">
        /// [out] S is REAL array of dimension N.
        /// The singular values of A, ordered so that S(i) &gt;= S(i+1).
        /// </param>
        /// <param name="u">
        /// [out] U is REAL array, dimension.
        /// LDU x M if JOBU = &#39;A&#39;; see the description of LDU. In this case,
        /// on exit, U contains the M left singular vectors.
        /// LDU x N if JOBU = &#39;S&#39;, &#39;U&#39;, &#39;R&#39; ; see the description of LDU. In this
        /// case, U contains the leading N or the leading NUMRANK left singular vectors.
        /// LDU x N if JOBU = &#39;F&#39; ; see the description of LDU. In this case U
        /// contains N x N orthogonal matrix that can be used to form the left
        /// singular vectors.
        /// If JOBU = &#39;N&#39;, U is not referenced.
        /// </param>
        /// <param name="ldu">
        /// [in] LDU is INTEGER.
        /// The leading dimension of the array U.
        /// If JOBU = &#39;A&#39;, &#39;S&#39;, &#39;U&#39;, &#39;R&#39;,  LDU &gt;= max(1,M).
        /// If JOBU = &#39;F&#39;,                 LDU &gt;= max(1,N).
        /// Otherwise,                     LDU &gt;= 1.
        /// </param>
        /// <param name="v">
        /// [out] V is REAL array, dimension.
        /// LDV x N if JOBV = &#39;A&#39;, &#39;V&#39;, &#39;R&#39; or if JOBA = &#39;E&#39; .
        /// If JOBV = &#39;A&#39;, or &#39;V&#39;,  V contains the N-by-N orthogonal matrix  V**T;
        /// If JOBV = &#39;R&#39;, V contains the first NUMRANK rows of V**T (the right
        /// singular vectors, stored rowwise, of the NUMRANK largest singular values).
        /// If JOBV = &#39;N&#39; and JOBA = &#39;E&#39;, V is used as a workspace.
        /// If JOBV = &#39;N&#39;, and JOBA.NE.&#39;E&#39;, V is not referenced.
        /// </param>
        /// <param name="ldv">
        /// [in] LDV is INTEGER.
        /// The leading dimension of the array V.
        /// If JOBV = &#39;A&#39;, &#39;V&#39;, &#39;R&#39;,  or JOBA = &#39;E&#39;, LDV &gt;= max(1,N).
        /// Otherwise,                               LDV &gt;= 1.
        /// </param>
        /// <param name="numrank">
        /// [out] NUMRANK is INTEGER.
        /// NUMRANK is the numerical rank first determined after the rank
        /// revealing QR factorization, following the strategy specified by the
        /// value of JOBA. If JOBV = &#39;R&#39; and JOBU = &#39;R&#39;, only NUMRANK
        /// leading singular values and vectors are then requested in the call
        /// of SGESVD. The final value of NUMRANK might be further reduced if
        /// some singular values are computed as zeros.
        /// </param>
        /// <returns>
        /// = 0:  successful exit.
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value.
        /// &gt; 0:  if SBDSQR did not converge, INFO specifies how many superdiagonals
        /// of an intermediate bidiagonal form B (computed in SGESVD) did not
        /// converge to zero.
        /// </returns>
        /// <remarks>
        /// <para>
        ///   1. The data movement (matrix transpose) is coded using simple nested
        ///   DO-loops because BLAS and LAPACK do not provide corresponding subroutines.
        ///   Those DO-loops are easily identified in this source code - by the CONTINUE
        ///   statements labeled with 11**. In an optimized version of this code, the
        ///   nested DO loops should be replaced with calls to an optimized subroutine.
        ///   2. This code scales A by 1/SQRT(M) if the largest ABS(A(i,j)) could cause
        ///   column norm overflow. This is the minial precaution and it is left to the
        ///   SVD routine (CGESVD) to do its own preemptive scaling if potential over-
        ///   or underflows are detected. To avoid repeated scanning of the array A,
        ///   an optimal implementation would do all necessary scaling before calling
        ///   CGESVD and the scaling in CGESVD can be switched off.
        ///   3. Other comments related to code optimization are given in comments in the
        ///   code, enclosed in [[double brackets]].
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_sgesvdq", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Sgesvdq(
            MatrixLayout matrixLayout,
            char joba,
            char jobp,
            char jobr,
            char jobu,
            char jobv,
            int m,
            int n,
            float* a,
            int lda,
            float* s,
            float* u,
            int ldu,
            float* v,
            int ldv,
            int* numrank);
    }
}
