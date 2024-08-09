using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DGGEVX computes for a pair of N-by-N real nonsymmetric matrices (A,B)
        /// the generalized eigenvalues, and optionally, the left and/or right
        /// generalized eigenvectors.
        /// </para>
        /// <para>
        /// Optionally also, it computes a balancing transformation to improve
        /// the conditioning of the eigenvalues and eigenvectors (ILO, IHI,
        /// LSCALE, RSCALE, ABNRM, and BBNRM), reciprocal condition numbers for
        /// the eigenvalues (RCONDE), and reciprocal condition numbers for the
        /// right eigenvectors (RCONDV).
        /// </para>
        /// <para>
        /// A generalized eigenvalue for a pair of matrices (A,B) is a scalar
        /// lambda or a ratio alpha/beta = lambda, such that A - lambda*B is
        /// singular. It is usually represented as the pair (alpha,beta), as
        /// there is a reasonable interpretation for beta=0, and even for both
        /// being zero.
        /// </para>
        /// <para>
        /// The right eigenvector v(j) corresponding to the eigenvalue lambda(j)
        /// of (A,B) satisfies
        /// </para>
        /// <para>
        ///                  A * v(j) = lambda(j) * B * v(j) .
        /// </para>
        /// <para>
        /// The left eigenvector u(j) corresponding to the eigenvalue lambda(j)
        /// of (A,B) satisfies
        /// </para>
        /// <para>
        ///                  u(j)**H * A  = lambda(j) * u(j)**H * B.
        /// </para>
        /// <para>
        /// where u(j)**H is the conjugate-transpose of u(j).
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="balanc">
        /// [in] BALANC is CHARACTER*1.
        /// Specifies the balance option to be performed.
        /// = &#39;N&#39;:  do not diagonally scale or permute;
        /// = &#39;P&#39;:  permute only;
        /// = &#39;S&#39;:  scale only;
        /// = &#39;B&#39;:  both permute and scale.
        /// Computed reciprocal condition numbers will be for the
        /// matrices after permuting and/or balancing. Permuting does
        /// not change condition numbers (in exact arithmetic), but
        /// balancing does.
        /// </param>
        /// <param name="jobvl">
        /// [in] JOBVL is CHARACTER*1.
        /// = &#39;N&#39;:  do not compute the left generalized eigenvectors;
        /// = &#39;V&#39;:  compute the left generalized eigenvectors.
        /// </param>
        /// <param name="jobvr">
        /// [in] JOBVR is CHARACTER*1.
        /// = &#39;N&#39;:  do not compute the right generalized eigenvectors;
        /// = &#39;V&#39;:  compute the right generalized eigenvectors.
        /// </param>
        /// <param name="sense">
        /// [in] SENSE is CHARACTER*1.
        /// Determines which reciprocal condition numbers are computed.
        /// = &#39;N&#39;: none are computed;
        /// = &#39;E&#39;: computed for eigenvalues only;
        /// = &#39;V&#39;: computed for eigenvectors only;
        /// = &#39;B&#39;: computed for eigenvalues and eigenvectors.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrices A, B, VL, and VR.  N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is DOUBLE PRECISION array, dimension (LDA, N).
        /// On entry, the matrix A in the pair (A,B).
        /// On exit, A has been overwritten. If JOBVL=&#39;V&#39; or JOBVR=&#39;V&#39;
        /// or both, then A contains the first part of the real Schur
        /// form of the &quot;balanced&quot; versions of the input A and B.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="b">
        /// [in,out] B is DOUBLE PRECISION array, dimension (LDB, N).
        /// On entry, the matrix B in the pair (A,B).
        /// On exit, B has been overwritten. If JOBVL=&#39;V&#39; or JOBVR=&#39;V&#39;
        /// or both, then B contains the second part of the real Schur
        /// form of the &quot;balanced&quot; versions of the input A and B.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of B.  LDB &gt;= max(1,N).
        /// </param>
        /// <param name="alphar">
        /// [out] ALPHAR is DOUBLE PRECISION array, dimension (N).
        /// </param>
        /// <param name="alphai">
        /// [out] ALPHAI is DOUBLE PRECISION array, dimension (N).
        /// </param>
        /// <param name="beta">
        /// [out] BETA is DOUBLE PRECISION array, dimension (N).
        /// On exit, (ALPHAR(j) + ALPHAI(j)*i)/BETA(j), j=1,...,N, will
        /// be the generalized eigenvalues.  If ALPHAI(j) is zero, then
        /// the j-th eigenvalue is real; if positive, then the j-th and
        /// (j+1)-st eigenvalues are a complex conjugate pair, with
        /// ALPHAI(j+1) negative.
        /// 
        /// Note: the quotients ALPHAR(j)/BETA(j) and ALPHAI(j)/BETA(j)
        /// may easily over- or underflow, and BETA(j) may even be zero.
        /// Thus, the user should avoid naively computing the ratio
        /// ALPHA/BETA. However, ALPHAR and ALPHAI will be always less
        /// than and usually comparable with norm(A) in magnitude, and
        /// BETA always less than and usually comparable with norm(B).
        /// </param>
        /// <param name="vl">
        /// [out] VL is DOUBLE PRECISION array, dimension (LDVL,N).
        /// If JOBVL = &#39;V&#39;, the left eigenvectors u(j) are stored one
        /// after another in the columns of VL, in the same order as
        /// their eigenvalues. If the j-th eigenvalue is real, then
        /// u(j) = VL(:,j), the j-th column of VL. If the j-th and
        /// (j+1)-th eigenvalues form a complex conjugate pair, then
        /// u(j) = VL(:,j)+i*VL(:,j+1) and u(j+1) = VL(:,j)-i*VL(:,j+1).
        /// Each eigenvector will be scaled so the largest component have
        /// abs(real part) + abs(imag. part) = 1.
        /// Not referenced if JOBVL = &#39;N&#39;.
        /// </param>
        /// <param name="ldvl">
        /// [in] LDVL is INTEGER.
        /// The leading dimension of the matrix VL. LDVL &gt;= 1, and
        /// if JOBVL = &#39;V&#39;, LDVL &gt;= N.
        /// </param>
        /// <param name="vr">
        /// [out] VR is DOUBLE PRECISION array, dimension (LDVR,N).
        /// If JOBVR = &#39;V&#39;, the right eigenvectors v(j) are stored one
        /// after another in the columns of VR, in the same order as
        /// their eigenvalues. If the j-th eigenvalue is real, then
        /// v(j) = VR(:,j), the j-th column of VR. If the j-th and
        /// (j+1)-th eigenvalues form a complex conjugate pair, then
        /// v(j) = VR(:,j)+i*VR(:,j+1) and v(j+1) = VR(:,j)-i*VR(:,j+1).
        /// Each eigenvector will be scaled so the largest component have
        /// abs(real part) + abs(imag. part) = 1.
        /// Not referenced if JOBVR = &#39;N&#39;.
        /// </param>
        /// <param name="ldvr">
        /// [in] LDVR is INTEGER.
        /// The leading dimension of the matrix VR. LDVR &gt;= 1, and
        /// if JOBVR = &#39;V&#39;, LDVR &gt;= N.
        /// </param>
        /// <param name="ilo">
        /// [out] ILO is INTEGER.
        /// </param>
        /// <param name="ihi">
        /// [out] IHI is INTEGER.
        /// ILO and IHI are integer values such that on exit
        /// A(i,j) = 0 and B(i,j) = 0 if i &gt; j and
        /// j = 1,...,ILO-1 or i = IHI+1,...,N.
        /// If BALANC = &#39;N&#39; or &#39;S&#39;, ILO = 1 and IHI = N.
        /// </param>
        /// <param name="lscale">
        /// [out] LSCALE is DOUBLE PRECISION array, dimension (N).
        /// Details of the permutations and scaling factors applied
        /// to the left side of A and B.  If PL(j) is the index of the
        /// row interchanged with row j, and DL(j) is the scaling
        /// factor applied to row j, then
        /// LSCALE(j) = PL(j)  for j = 1,...,ILO-1
        /// = DL(j)  for j = ILO,...,IHI
        /// = PL(j)  for j = IHI+1,...,N.
        /// The order in which the interchanges are made is N to IHI+1,
        /// then 1 to ILO-1.
        /// </param>
        /// <param name="rscale">
        /// [out] RSCALE is DOUBLE PRECISION array, dimension (N).
        /// Details of the permutations and scaling factors applied
        /// to the right side of A and B.  If PR(j) is the index of the
        /// column interchanged with column j, and DR(j) is the scaling
        /// factor applied to column j, then
        /// RSCALE(j) = PR(j)  for j = 1,...,ILO-1
        /// = DR(j)  for j = ILO,...,IHI
        /// = PR(j)  for j = IHI+1,...,N
        /// The order in which the interchanges are made is N to IHI+1,
        /// then 1 to ILO-1.
        /// </param>
        /// <param name="abnrm">
        /// [out] ABNRM is DOUBLE PRECISION.
        /// The one-norm of the balanced matrix A.
        /// </param>
        /// <param name="bbnrm">
        /// [out] BBNRM is DOUBLE PRECISION.
        /// The one-norm of the balanced matrix B.
        /// </param>
        /// <param name="rconde">
        /// [out] RCONDE is DOUBLE PRECISION array, dimension (N).
        /// If SENSE = &#39;E&#39; or &#39;B&#39;, the reciprocal condition numbers of
        /// the eigenvalues, stored in consecutive elements of the array.
        /// For a complex conjugate pair of eigenvalues two consecutive
        /// elements of RCONDE are set to the same value. Thus RCONDE(j),
        /// RCONDV(j), and the j-th columns of VL and VR all correspond
        /// to the j-th eigenpair.
        /// If SENSE = &#39;N or &#39;V&#39;, RCONDE is not referenced.
        /// </param>
        /// <param name="rcondv">
        /// [out] RCONDV is DOUBLE PRECISION array, dimension (N).
        /// If SENSE = &#39;V&#39; or &#39;B&#39;, the estimated reciprocal condition
        /// numbers of the eigenvectors, stored in consecutive elements
        /// of the array. For a complex eigenvector two consecutive
        /// elements of RCONDV are set to the same value. If the
        /// eigenvalues cannot be reordered to compute RCONDV(j),
        /// RCONDV(j) is set to 0; this can only occur when the true
        /// value would be very small anyway.
        /// If SENSE = &#39;N&#39; or &#39;E&#39;, RCONDV is not referenced.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value.
        /// = 1,...,N:
        /// The QZ iteration failed.  No eigenvectors have been
        /// calculated, but ALPHAR(j), ALPHAI(j), and BETA(j)
        /// should be correct for j=INFO+1,...,N.
        /// &gt; N:  =N+1: other than QZ iteration failed in DHGEQZ.
        /// =N+2: error return from DTGEVC.
        /// </returns>
        /// <remarks>
        /// <para>
        ///  Balancing a matrix pair (A,B) includes, first, permuting rows and
        ///  columns to isolate eigenvalues, second, applying diagonal similarity
        ///  transformation to the rows and columns to make the rows and columns
        ///  as close in norm as possible. The computed reciprocal condition
        ///  numbers correspond to the balanced matrix. Permuting rows and columns
        ///  will not change the condition numbers (in exact arithmetic) but
        ///  diagonal scaling will.  For further explanation of balancing, see
        ///  section 4.11.1.2 of LAPACK Users&#39; Guide.
        /// </para>
        /// <para>
        ///  An approximate error bound on the chordal distance between the i-th
        ///  computed generalized eigenvalue w and the corresponding exact
        ///  eigenvalue lambda is
        /// </para>
        /// <para>
        ///       chord(w, lambda) &lt;= EPS * norm(ABNRM, BBNRM) / RCONDE(I)
        /// </para>
        /// <para>
        ///  An approximate error bound for the angle between the i-th computed
        ///  eigenvector VL(i) or VR(i) is given by
        /// </para>
        /// <para>
        ///       EPS * norm(ABNRM, BBNRM) / DIF(i).
        /// </para>
        /// <para>
        ///  For further explanation of the reciprocal condition numbers RCONDE
        ///  and RCONDV, see section 4.11 of LAPACK User&#39;s Guide.
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dggevx", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dggevx(
            MatrixLayout matrixLayout,
            char balanc,
            char jobvl,
            char jobvr,
            char sense,
            int n,
            double* a,
            int lda,
            double* b,
            int ldb,
            double* alphar,
            double* alphai,
            double* beta,
            double* vl,
            int ldvl,
            double* vr,
            int ldvr,
            int* ilo,
            int* ihi,
            double* lscale,
            double* rscale,
            double* abnrm,
            double* bbnrm,
            double* rconde,
            double* rcondv);
    }
}
