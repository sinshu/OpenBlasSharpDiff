using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CGEJSV computes the singular value decomposition (SVD) of a complex M-by-N
        /// matrix [A], where M &gt;= N. The SVD of [A] is written as
        /// </para>
        /// <para>
        ///              [A] = [U] * [SIGMA] * [V]^*,
        /// </para>
        /// <para>
        /// where [SIGMA] is an N-by-N (M-by-N) matrix which is zero except for its N
        /// diagonal elements, [U] is an M-by-N (or M-by-M) unitary matrix, and
        /// [V] is an N-by-N unitary matrix. The diagonal elements of [SIGMA] are
        /// the singular values of [A]. The columns of [U] and [V] are the left and
        /// the right singular vectors of [A], respectively. The matrices [U] and [V]
        /// are computed and stored in the arrays U and V, respectively. The diagonal
        /// of [SIGMA] is computed and stored in the array SVA.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="joba">
        /// [in] JOBA is CHARACTER*1.
        /// Specifies the level of accuracy:
        /// = &#39;C&#39;: This option works well (high relative accuracy) if A = B * D,
        /// with well-conditioned B and arbitrary diagonal matrix D.
        /// The accuracy cannot be spoiled by COLUMN scaling. The
        /// accuracy of the computed output depends on the condition of
        /// B, and the procedure aims at the best theoretical accuracy.
        /// The relative error max_{i=1:N}|d sigma_i| / sigma_i is
        /// bounded by f(M,N)*epsilon* cond(B), independent of D.
        /// The input matrix is preprocessed with the QRF with column
        /// pivoting. This initial preprocessing and preconditioning by
        /// a rank revealing QR factorization is common for all values of
        /// JOBA. Additional actions are specified as follows:
        /// = &#39;E&#39;: Computation as with &#39;C&#39; with an additional estimate of the
        /// condition number of B. It provides a realistic error bound.
        /// = &#39;F&#39;: If A = D1 * C * D2 with ill-conditioned diagonal scalings
        /// D1, D2, and well-conditioned matrix C, this option gives
        /// higher accuracy than the &#39;C&#39; option. If the structure of the
        /// input matrix is not known, and relative accuracy is
        /// desirable, then this option is advisable. The input matrix A
        /// is preprocessed with QR factorization with FULL (row and
        /// column) pivoting.
        /// = &#39;G&#39;: Computation as with &#39;F&#39; with an additional estimate of the
        /// condition number of B, where A=B*D. If A has heavily weighted
        /// rows, then using this condition number gives too pessimistic
        /// error bound.
        /// = &#39;A&#39;: Small singular values are not well determined by the data
        /// and are considered as noisy; the matrix is treated as
        /// numerically rank deficient. The error in the computed
        /// singular values is bounded by f(m,n)*epsilon*||A||.
        /// The computed SVD A = U * S * V^* restores A up to
        /// f(m,n)*epsilon*||A||.
        /// This gives the procedure the licence to discard (set to zero)
        /// all singular values below N*epsilon*||A||.
        /// = &#39;R&#39;: Similar as in &#39;A&#39;. Rank revealing property of the initial
        /// QR factorization is used do reveal (using triangular factor)
        /// a gap sigma_{r+1} &lt; epsilon * sigma_r in which case the
        /// numerical RANK is declared to be r. The SVD is computed with
        /// absolute error bounds, but more accurately than with &#39;A&#39;.
        /// </param>
        /// <param name="jobu">
        /// [in] JOBU is CHARACTER*1.
        /// Specifies whether to compute the columns of U:
        /// = &#39;U&#39;: N columns of U are returned in the array U.
        /// = &#39;F&#39;: full set of M left sing. vectors is returned in the array U.
        /// = &#39;W&#39;: U may be used as workspace of length M*N. See the description
        /// of U.
        /// = &#39;N&#39;: U is not computed.
        /// </param>
        /// <param name="jobv">
        /// [in] JOBV is CHARACTER*1.
        /// Specifies whether to compute the matrix V:
        /// = &#39;V&#39;: N columns of V are returned in the array V; Jacobi rotations
        /// are not explicitly accumulated.
        /// = &#39;J&#39;: N columns of V are returned in the array V, but they are
        /// computed as the product of Jacobi rotations, if JOBT = &#39;N&#39;.
        /// = &#39;W&#39;: V may be used as workspace of length N*N. See the description
        /// of V.
        /// = &#39;N&#39;: V is not computed.
        /// </param>
        /// <param name="jobr">
        /// [in] JOBR is CHARACTER*1.
        /// Specifies the RANGE for the singular values. Issues the licence to
        /// set to zero small positive singular values if they are outside
        /// specified range. If A .NE. 0 is scaled so that the largest singular
        /// value of c*A is around SQRT(BIG), BIG=SLAMCH(&#39;O&#39;), then JOBR issues
        /// the licence to kill columns of A whose norm in c*A is less than
        /// SQRT(SFMIN) (for JOBR = &#39;R&#39;), or less than SMALL=SFMIN/EPSLN,
        /// where SFMIN=SLAMCH(&#39;S&#39;), EPSLN=SLAMCH(&#39;E&#39;).
        /// = &#39;N&#39;: Do not kill small columns of c*A. This option assumes that
        /// BLAS and QR factorizations and triangular solvers are
        /// implemented to work in that range. If the condition of A
        /// is greater than BIG, use CGESVJ.
        /// = &#39;R&#39;: RESTRICTED range for sigma(c*A) is [SQRT(SFMIN), SQRT(BIG)]
        /// (roughly, as described above). This option is recommended.
        /// ===========================
        /// For computing the singular values in the FULL range [SFMIN,BIG]
        /// use CGESVJ.
        /// </param>
        /// <param name="jobt">
        /// [in] JOBT is CHARACTER*1.
        /// If the matrix is square then the procedure may determine to use
        /// transposed A if A^* seems to be better with respect to convergence.
        /// If the matrix is not square, JOBT is ignored.
        /// The decision is based on two values of entropy over the adjoint
        /// orbit of A^* * A. See the descriptions of RWORK(6) and RWORK(7).
        /// = &#39;T&#39;: transpose if entropy test indicates possibly faster
        /// convergence of Jacobi process if A^* is taken as input. If A is
        /// replaced with A^*, then the row pivoting is included automatically.
        /// = &#39;N&#39;: do not speculate.
        /// The option &#39;T&#39; can be used to compute only the singular values, or
        /// the full SVD (U, SIGMA and V). For only one set of singular vectors
        /// (U or V), the caller should provide both U and V, as one of the
        /// matrices is used as workspace if the matrix A is transposed.
        /// The implementer can easily remove this constraint and make the
        /// code more complicated. See the descriptions of U and V.
        /// In general, this option is considered experimental, and &#39;N&#39;; should
        /// be preferred. This is subject to changes in the future.
        /// </param>
        /// <param name="jobp">
        /// [in] JOBP is CHARACTER*1.
        /// Issues the licence to introduce structured perturbations to drown
        /// denormalized numbers. This licence should be active if the
        /// denormals are poorly implemented, causing slow computation,
        /// especially in cases of fast convergence (!). For details see [1,2].
        /// For the sake of simplicity, this perturbations are included only
        /// when the full SVD or only the singular values are requested. The
        /// implementer/user can easily add the perturbation for the cases of
        /// computing one set of singular vectors.
        /// = &#39;P&#39;: introduce perturbation
        /// = &#39;N&#39;: do not perturb
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of rows of the input matrix A.  M &gt;= 0.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of columns of the input matrix A. M &gt;= N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is COMPLEX array, dimension (LDA,N).
        /// On entry, the M-by-N matrix A.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,M).
        /// </param>
        /// <param name="sva">
        /// [out] SVA is REAL array, dimension (N).
        /// On exit,
        /// - For RWORK(1)/RWORK(2) = ONE: The singular values of A. During
        /// the computation SVA contains Euclidean column norms of the
        /// iterated matrices in the array A.
        /// - For RWORK(1) .NE. RWORK(2): The singular values of A are
        /// (RWORK(1)/RWORK(2)) * SVA(1:N). This factored form is used if
        /// sigma_max(A) overflows or if small singular values have been
        /// saved from underflow by scaling the input matrix A.
        /// - If JOBR=&#39;R&#39; then some of the singular values may be returned
        /// as exact zeros obtained by &quot;set to zero&quot; because they are
        /// below the numerical rank threshold or are denormalized numbers.
        /// </param>
        /// <param name="u">
        /// [out] U is COMPLEX array, dimension ( LDU, N ) or ( LDU, M ).
        /// If JOBU = &#39;U&#39;, then U contains on exit the M-by-N matrix of
        /// the left singular vectors.
        /// If JOBU = &#39;F&#39;, then U contains on exit the M-by-M matrix of
        /// the left singular vectors, including an ONB
        /// of the orthogonal complement of the Range(A).
        /// If JOBU = &#39;W&#39;  .AND. (JOBV = &#39;V&#39; .AND. JOBT = &#39;T&#39; .AND. M = N),
        /// then U is used as workspace if the procedure
        /// replaces A with A^*. In that case, [V] is computed
        /// in U as left singular vectors of A^* and then
        /// copied back to the V array. This &#39;W&#39; option is just
        /// a reminder to the caller that in this case U is
        /// reserved as workspace of length N*N.
        /// If JOBU = &#39;N&#39;  U is not referenced, unless JOBT=&#39;T&#39;.
        /// </param>
        /// <param name="ldu">
        /// [in] LDU is INTEGER.
        /// The leading dimension of the array U,  LDU &gt;= 1.
        /// IF  JOBU = &#39;U&#39; or &#39;F&#39; or &#39;W&#39;,  then LDU &gt;= M.
        /// </param>
        /// <param name="v">
        /// [out] V is COMPLEX array, dimension ( LDV, N ).
        /// If JOBV = &#39;V&#39;, &#39;J&#39; then V contains on exit the N-by-N matrix of
        /// the right singular vectors;
        /// If JOBV = &#39;W&#39;, AND (JOBU = &#39;U&#39; AND JOBT = &#39;T&#39; AND M = N),
        /// then V is used as workspace if the procedure
        /// replaces A with A^*. In that case, [U] is computed
        /// in V as right singular vectors of A^* and then
        /// copied back to the U array. This &#39;W&#39; option is just
        /// a reminder to the caller that in this case V is
        /// reserved as workspace of length N*N.
        /// If JOBV = &#39;N&#39;  V is not referenced, unless JOBT=&#39;T&#39;.
        /// </param>
        /// <param name="ldv">
        /// [in] LDV is INTEGER.
        /// The leading dimension of the array V,  LDV &gt;= 1.
        /// If JOBV = &#39;V&#39; or &#39;J&#39; or &#39;W&#39;, then LDV &gt;= N.
        /// </param>
        /// <param name="stat">
        /// No description available.
        /// </param>
        /// <param name="istat">
        /// No description available.
        /// </param>
        /// <returns>
        /// &lt; 0:  if INFO = -i, then the i-th argument had an illegal value.
        /// = 0:  successful exit;
        /// &gt; 0:  CGEJSV  did not converge in the maximal allowed number
        /// of sweeps. The computed values may be inaccurate.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_cgejsv", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Cgejsv(
            MatrixLayout matrixLayout,
            char joba,
            char jobu,
            char jobv,
            char jobr,
            char jobt,
            char jobp,
            int m,
            int n,
            Complex32* a,
            int lda,
            float* sva,
            Complex32* u,
            int ldu,
            Complex32* v,
            int ldv,
            float* stat,
            int* istat);
    }
}
