using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DGEJSV computes the singular value decomposition (SVD) of a real M-by-N
        /// matrix [A], where M &gt;= N. The SVD of [A] is written as
        /// </para>
        /// <para>
        ///              [A] = [U] * [SIGMA] * [V]^t,
        /// </para>
        /// <para>
        /// where [SIGMA] is an N-by-N (M-by-N) matrix which is zero except for its N
        /// diagonal elements, [U] is an M-by-N (or M-by-M) orthonormal matrix, and
        /// [V] is an N-by-N orthogonal matrix. The diagonal elements of [SIGMA] are
        /// the singular values of [A]. The columns of [U] and [V] are the left and
        /// the right singular vectors of [A], respectively. The matrices [U] and [V]
        /// are computed and stored in the arrays U and V, respectively. The diagonal
        /// of [SIGMA] is computed and stored in the array SVA.
        /// DGEJSV can sometimes compute tiny singular values and their singular vectors much
        /// more accurately than other SVD routines, see below under Further Details.
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
        /// condition number of B, where A=D*B. If A has heavily weighted
        /// rows, then using this condition number gives too pessimistic
        /// error bound.
        /// = &#39;A&#39;: Small singular values are the noise and the matrix is treated
        /// as numerically rank deficient. The error in the computed
        /// singular values is bounded by f(m,n)*epsilon*||A||.
        /// The computed SVD A = U * S * V^t restores A up to
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
        /// computed as the product of Jacobi rotations. This option is
        /// allowed only if JOBU .NE. &#39;N&#39;, i.e. in computing the full SVD.
        /// = &#39;W&#39;: V may be used as workspace of length N*N. See the description
        /// of V.
        /// = &#39;N&#39;: V is not computed.
        /// </param>
        /// <param name="jobr">
        /// [in] JOBR is CHARACTER*1.
        /// Specifies the RANGE for the singular values. Issues the licence to
        /// set to zero small positive singular values if they are outside
        /// specified range. If A .NE. 0 is scaled so that the largest singular
        /// value of c*A is around DSQRT(BIG), BIG=SLAMCH(&#39;O&#39;), then JOBR issues
        /// the licence to kill columns of A whose norm in c*A is less than
        /// DSQRT(SFMIN) (for JOBR = &#39;R&#39;), or less than SMALL=SFMIN/EPSLN,
        /// where SFMIN=SLAMCH(&#39;S&#39;), EPSLN=SLAMCH(&#39;E&#39;).
        /// = &#39;N&#39;: Do not kill small columns of c*A. This option assumes that
        /// BLAS and QR factorizations and triangular solvers are
        /// implemented to work in that range. If the condition of A
        /// is greater than BIG, use DGESVJ.
        /// = &#39;R&#39;: RESTRICTED range for sigma(c*A) is [DSQRT(SFMIN), DSQRT(BIG)]
        /// (roughly, as described above). This option is recommended.
        /// ~~~~~~~~~~~~~~~~~~~~~~~~~~~
        /// For computing the singular values in the FULL range [SFMIN,BIG]
        /// use DGESVJ.
        /// </param>
        /// <param name="jobt">
        /// [in] JOBT is CHARACTER*1.
        /// If the matrix is square then the procedure may determine to use
        /// transposed A if A^t seems to be better with respect to convergence.
        /// If the matrix is not square, JOBT is ignored. This is subject to
        /// changes in the future.
        /// The decision is based on two values of entropy over the adjoint
        /// orbit of A^t * A. See the descriptions of WORK(6) and WORK(7).
        /// = &#39;T&#39;: transpose if entropy test indicates possibly faster
        /// convergence of Jacobi process if A^t is taken as input. If A is
        /// replaced with A^t, then the row pivoting is included automatically.
        /// = &#39;N&#39;: do not speculate.
        /// This option can be used to compute only the singular values, or the
        /// full SVD (U, SIGMA and V). For only one set of singular vectors
        /// (U or V), the caller should provide both U and V, as one of the
        /// matrices is used as workspace if the matrix A is transposed.
        /// The implementer can easily remove this constraint and make the
        /// code more complicated. See the descriptions of U and V.
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
        /// [in,out] A is DOUBLE PRECISION array, dimension (LDA,N).
        /// On entry, the M-by-N matrix A.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,M).
        /// </param>
        /// <param name="sva">
        /// [out] SVA is DOUBLE PRECISION array, dimension (N).
        /// On exit,
        /// - For WORK(1)/WORK(2) = ONE: The singular values of A. During the
        /// computation SVA contains Euclidean column norms of the
        /// iterated matrices in the array A.
        /// - For WORK(1) .NE. WORK(2): The singular values of A are
        /// (WORK(1)/WORK(2)) * SVA(1:N). This factored form is used if
        /// sigma_max(A) overflows or if small singular values have been
        /// saved from underflow by scaling the input matrix A.
        /// - If JOBR=&#39;R&#39; then some of the singular values may be returned
        /// as exact zeros obtained by &quot;set to zero&quot; because they are
        /// below the numerical rank threshold or are denormalized numbers.
        /// </param>
        /// <param name="u">
        /// [out] U is DOUBLE PRECISION array, dimension ( LDU, N ) or ( LDU, M ).
        /// If JOBU = &#39;U&#39;, then U contains on exit the M-by-N matrix of
        /// the left singular vectors.
        /// If JOBU = &#39;F&#39;, then U contains on exit the M-by-M matrix of
        /// the left singular vectors, including an ONB
        /// of the orthogonal complement of the Range(A).
        /// If JOBU = &#39;W&#39;  .AND. (JOBV = &#39;V&#39; .AND. JOBT = &#39;T&#39; .AND. M = N),
        /// then U is used as workspace if the procedure
        /// replaces A with A^t. In that case, [V] is computed
        /// in U as left singular vectors of A^t and then
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
        /// [out] V is DOUBLE PRECISION array, dimension ( LDV, N ).
        /// If JOBV = &#39;V&#39;, &#39;J&#39; then V contains on exit the N-by-N matrix of
        /// the right singular vectors;
        /// If JOBV = &#39;W&#39;, AND (JOBU = &#39;U&#39; AND JOBT = &#39;T&#39; AND M = N),
        /// then V is used as workspace if the procedure
        /// replaces A with A^t. In that case, [U] is computed
        /// in V as right singular vectors of A^t and then
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
        /// &gt; 0:  DGEJSV  did not converge in the maximal allowed number
        /// of sweeps. The computed values may be inaccurate.
        /// </returns>
        /// <remarks>
        /// <para>
        ///  DGEJSV implements a preconditioned Jacobi SVD algorithm. It uses DGEQP3,
        ///  DGEQRF, and DGELQF as preprocessors and preconditioners. Optionally, an
        ///  additional row pivoting can be used as a preprocessor, which in some
        ///  cases results in much higher accuracy. An example is matrix A with the
        ///  structure A = D1 * C * D2, where D1, D2 are arbitrarily ill-conditioned
        ///  diagonal matrices and C is well-conditioned matrix. In that case, complete
        ///  pivoting in the first QR factorizations provides accuracy dependent on the
        ///  condition number of C, and independent of D1, D2. Such higher accuracy is
        ///  not completely understood theoretically, but it works well in practice.
        ///  Further, if A can be written as A = B*D, with well-conditioned B and some
        ///  diagonal D, then the high accuracy is guaranteed, both theoretically and
        ///  in software, independent of D. For more details see [1], [2].
        ///     The computational range for the singular values can be the full range
        ///  ( UNDERFLOW,OVERFLOW ), provided that the machine arithmetic and the BLAS
        ///  &amp; LAPACK routines called by DGEJSV are implemented to work in that range.
        ///  If that is not the case, then the restriction for safe computation with
        ///  the singular values in the range of normalized IEEE numbers is that the
        ///  spectral condition number kappa(A)=sigma_max(A)/sigma_min(A) does not
        ///  overflow. This code (DGEJSV) is best used in this restricted range,
        ///  meaning that singular values of magnitude below ||A||_2 / DLAMCH(&#39;O&#39;) are
        ///  returned as zeros. See JOBR for details on this.
        ///     Further, this implementation is somewhat slower than the one described
        ///  in [1,2] due to replacement of some non-LAPACK components, and because
        ///  the choice of some tuning parameters in the iterative part (DGESVJ) is
        ///  left to the implementer on a particular machine.
        ///     The rank revealing QR factorization (in this code: DGEQP3) should be
        ///  implemented as in [3]. We have a new version of DGEQP3 under development
        ///  that is more robust than the current one in LAPACK, with a cleaner cut in
        ///  rank deficient cases. It will be available in the SIGMA library [4].
        ///  If M is much larger than N, it is obvious that the initial QRF with
        ///  column pivoting can be preprocessed by the QRF without pivoting. That
        ///  well known trick is not used in DGEJSV because in some cases heavy row
        ///  weighting can be treated with complete pivoting. The overhead in cases
        ///  M much larger than N is then only due to pivoting, but the benefits in
        ///  terms of accuracy have prevailed. The implementer/user can incorporate
        ///  this extra QRF step easily. The implementer can also improve data movement
        ///  (matrix transpose, matrix copy, matrix transposed copy) - this
        ///  implementation of DGEJSV uses only the simplest, naive data movement.
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dgejsv", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dgejsv(
            MatrixLayout matrixLayout,
            char joba,
            char jobu,
            char jobv,
            char jobr,
            char jobt,
            char jobp,
            int m,
            int n,
            double* a,
            int lda,
            double* sva,
            double* u,
            int ldu,
            double* v,
            int ldv,
            double* stat,
            int* istat);
    }
}
