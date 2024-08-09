using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CGESVJ computes the singular value decomposition (SVD) of a complex
        /// M-by-N matrix A, where M &gt;= N. The SVD of A is written as
        ///                                    [++]   [xx]   [x0]   [xx]
        ///              A = U * SIGMA * V^*,  [++] = [xx] * [ox] * [xx]
        ///                                    [++]   [xx]
        /// where SIGMA is an N-by-N diagonal matrix, U is an M-by-N orthonormal
        /// matrix, and V is an N-by-N unitary matrix. The diagonal elements
        /// of SIGMA are the singular values of A. The columns of U and V are the
        /// left and the right singular vectors of A, respectively.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="joba">
        /// [in] JOBA is CHARACTER*1.
        /// Specifies the structure of A.
        /// = &#39;L&#39;: The input matrix A is lower triangular;
        /// = &#39;U&#39;: The input matrix A is upper triangular;
        /// = &#39;G&#39;: The input matrix A is general M-by-N matrix, M &gt;= N.
        /// </param>
        /// <param name="jobu">
        /// [in] JOBU is CHARACTER*1.
        /// Specifies whether to compute the left singular vectors
        /// (columns of U):
        /// = &#39;U&#39; or &#39;F&#39;: The left singular vectors corresponding to the nonzero
        /// singular values are computed and returned in the leading
        /// columns of A. See more details in the description of A.
        /// The default numerical orthogonality threshold is set to
        /// approximately TOL=CTOL*EPS, CTOL=SQRT(M), EPS=SLAMCH(&#39;E&#39;).
        /// = &#39;C&#39;: Analogous to JOBU=&#39;U&#39;, except that user can control the
        /// level of numerical orthogonality of the computed left
        /// singular vectors. TOL can be set to TOL = CTOL*EPS, where
        /// CTOL is given on input in the array WORK.
        /// No CTOL smaller than ONE is allowed. CTOL greater
        /// than 1 / EPS is meaningless. The option &#39;C&#39;
        /// can be used if M*EPS is satisfactory orthogonality
        /// of the computed left singular vectors, so CTOL=M could
        /// save few sweeps of Jacobi rotations.
        /// See the descriptions of A and WORK(1).
        /// = &#39;N&#39;: The matrix U is not computed. However, see the
        /// description of A.
        /// </param>
        /// <param name="jobv">
        /// [in] JOBV is CHARACTER*1.
        /// Specifies whether to compute the right singular vectors, that
        /// is, the matrix V:
        /// = &#39;V&#39; or &#39;J&#39;: the matrix V is computed and returned in the array V
        /// = &#39;A&#39;:  the Jacobi rotations are applied to the MV-by-N
        /// array V. In other words, the right singular vector
        /// matrix V is not computed explicitly; instead it is
        /// applied to an MV-by-N matrix initially stored in the
        /// first MV rows of V.
        /// = &#39;N&#39;:  the matrix V is not computed and the array V is not
        /// referenced
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of rows of the input matrix A. 1/SLAMCH(&#39;E&#39;) &gt; M &gt;= 0.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of columns of the input matrix A.
        /// M &gt;= N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is COMPLEX array, dimension (LDA,N).
        /// On entry, the M-by-N matrix A.
        /// On exit,
        /// If JOBU = &#39;U&#39; .OR. JOBU = &#39;C&#39;:
        /// If INFO = 0 :
        /// RANKA orthonormal columns of U are returned in the
        /// leading RANKA columns of the array A. Here RANKA &lt;= N
        /// is the number of computed singular values of A that are
        /// above the underflow threshold SLAMCH(&#39;S&#39;). The singular
        /// vectors corresponding to underflowed or zero singular
        /// values are not computed. The value of RANKA is returned
        /// in the array RWORK as RANKA=NINT(RWORK(2)). Also see the
        /// descriptions of SVA and RWORK. The computed columns of U
        /// are mutually numerically orthogonal up to approximately
        /// TOL=SQRT(M)*EPS (default); or TOL=CTOL*EPS (JOBU = &#39;C&#39;),
        /// see the description of JOBU.
        /// If INFO &gt; 0,
        /// the procedure CGESVJ did not converge in the given number
        /// of iterations (sweeps). In that case, the computed
        /// columns of U may not be orthogonal up to TOL. The output
        /// U (stored in A), SIGMA (given by the computed singular
        /// values in SVA(1:N)) and V is still a decomposition of the
        /// input matrix A in the sense that the residual
        /// || A - SCALE * U * SIGMA * V^* ||_2 / ||A||_2 is small.
        /// If JOBU = &#39;N&#39;:
        /// If INFO = 0 :
        /// Note that the left singular vectors are &#39;for free&#39; in the
        /// one-sided Jacobi SVD algorithm. However, if only the
        /// singular values are needed, the level of numerical
        /// orthogonality of U is not an issue and iterations are
        /// stopped when the columns of the iterated matrix are
        /// numerically orthogonal up to approximately M*EPS. Thus,
        /// on exit, A contains the columns of U scaled with the
        /// corresponding singular values.
        /// If INFO &gt; 0 :
        /// the procedure CGESVJ did not converge in the given number
        /// of iterations (sweeps).
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,M).
        /// </param>
        /// <param name="sva">
        /// [out] SVA is REAL array, dimension (N).
        /// On exit,
        /// If INFO = 0 :
        /// depending on the value SCALE = RWORK(1), we have:
        /// If SCALE = ONE:
        /// SVA(1:N) contains the computed singular values of A.
        /// During the computation SVA contains the Euclidean column
        /// norms of the iterated matrices in the array A.
        /// If SCALE .NE. ONE:
        /// The singular values of A are SCALE*SVA(1:N), and this
        /// factored representation is due to the fact that some of the
        /// singular values of A might underflow or overflow.
        /// 
        /// If INFO &gt; 0 :
        /// the procedure CGESVJ did not converge in the given number of
        /// iterations (sweeps) and SCALE*SVA(1:N) may not be accurate.
        /// </param>
        /// <param name="mv">
        /// [in] MV is INTEGER.
        /// If JOBV = &#39;A&#39;, then the product of Jacobi rotations in CGESVJ
        /// is applied to the first MV rows of V. See the description of JOBV.
        /// </param>
        /// <param name="v">
        /// [in,out] V is COMPLEX array, dimension (LDV,N).
        /// If JOBV = &#39;V&#39;, then V contains on exit the N-by-N matrix of
        /// the right singular vectors;
        /// If JOBV = &#39;A&#39;, then V contains the product of the computed right
        /// singular vector matrix and the initial matrix in
        /// the array V.
        /// If JOBV = &#39;N&#39;, then V is not referenced.
        /// </param>
        /// <param name="ldv">
        /// [in] LDV is INTEGER.
        /// The leading dimension of the array V, LDV &gt;= 1.
        /// If JOBV = &#39;V&#39;, then LDV &gt;= max(1,N).
        /// If JOBV = &#39;A&#39;, then LDV &gt;= max(1,MV) .
        /// </param>
        /// <param name="stat">
        /// No description available.
        /// </param>
        /// <returns>
        /// = 0:  successful exit.
        /// &lt; 0:  if INFO = -i, then the i-th argument had an illegal value
        /// &gt; 0:  CGESVJ did not converge in the maximal allowed number
        /// (NSWEEP=30) of sweeps. The output may still be useful.
        /// See the description of RWORK.
        /// </returns>
        /// <remarks>
        /// <para>
        /// The orthogonal N-by-N matrix V is obtained as a product of Jacobi plane
        /// rotations. In the case of underflow of the tangent of the Jacobi angle, a
        /// modified Jacobi transformation of Drmac [3] is used. Pivot strategy uses
        /// column interchanges of de Rijk [1]. The relative accuracy of the computed
        /// singular values and the accuracy of the computed singular vectors (in
        /// angle metric) is as guaranteed by the theory of Demmel and Veselic [2].
        /// The condition number that determines the accuracy in the full rank case
        /// is essentially min_{D=diag} kappa(A*D), where kappa(.) is the
        /// spectral condition number. The best performance of this Jacobi SVD
        /// procedure is achieved if used in an  accelerated version of Drmac and
        /// Veselic [4,5], and it is the kernel routine in the SIGMA library [6].
        /// Some tuning parameters (marked with [TP]) are available for the
        /// implementer.
        /// The computational range for the nonzero singular values is the  machine
        /// number interval ( UNDERFLOW , OVERFLOW ). In extreme cases, even
        /// denormalized singular values can be computed with the corresponding
        /// gradual loss of accurate digits.
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_cgesvj", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Cgesvj(
            MatrixLayout matrixLayout,
            char joba,
            char jobu,
            char jobv,
            int m,
            int n,
            Complex32* a,
            int lda,
            float* sva,
            int mv,
            Complex32* v,
            int ldv,
            float* stat);
    }
}
