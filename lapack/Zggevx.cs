using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZGGEVX computes for a pair of N-by-N complex nonsymmetric matrices
        /// (A,B) the generalized eigenvalues, and optionally, the left and/or
        /// right generalized eigenvectors.
        /// </para>
        /// <para>
        /// Optionally, it also computes a balancing transformation to improve
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
        ///                  A * v(j) = lambda(j) * B * v(j) .
        /// The left eigenvector u(j) corresponding to the eigenvalue lambda(j)
        /// of (A,B) satisfies
        ///                  u(j)**H * A  = lambda(j) * u(j)**H * B.
        /// where u(j)**H is the conjugate-transpose of u(j).
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="balanc">
        /// [in] BALANC is CHARACTER*1.
        /// Specifies the balance option to be performed:
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
        /// [in,out] A is COMPLEX*16 array, dimension (LDA, N).
        /// On entry, the matrix A in the pair (A,B).
        /// On exit, A has been overwritten. If JOBVL=&#39;V&#39; or JOBVR=&#39;V&#39;
        /// or both, then A contains the first part of the complex Schur
        /// form of the &quot;balanced&quot; versions of the input A and B.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="b">
        /// [in,out] B is COMPLEX*16 array, dimension (LDB, N).
        /// On entry, the matrix B in the pair (A,B).
        /// On exit, B has been overwritten. If JOBVL=&#39;V&#39; or JOBVR=&#39;V&#39;
        /// or both, then B contains the second part of the complex
        /// Schur form of the &quot;balanced&quot; versions of the input A and B.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of B.  LDB &gt;= max(1,N).
        /// </param>
        /// <param name="alpha">
        /// [out] ALPHA is COMPLEX*16 array, dimension (N).
        /// </param>
        /// <param name="beta">
        /// [out] BETA is COMPLEX*16 array, dimension (N).
        /// On exit, ALPHA(j)/BETA(j), j=1,...,N, will be the generalized
        /// eigenvalues.
        /// 
        /// Note: the quotient ALPHA(j)/BETA(j) ) may easily over- or
        /// underflow, and BETA(j) may even be zero.  Thus, the user
        /// should avoid naively computing the ratio ALPHA/BETA.
        /// However, ALPHA will be always less than and usually
        /// comparable with norm(A) in magnitude, and BETA always less
        /// than and usually comparable with norm(B).
        /// </param>
        /// <param name="vl">
        /// [out] VL is COMPLEX*16 array, dimension (LDVL,N).
        /// If JOBVL = &#39;V&#39;, the left generalized eigenvectors u(j) are
        /// stored one after another in the columns of VL, in the same
        /// order as their eigenvalues.
        /// Each eigenvector will be scaled so the largest component
        /// will have abs(real part) + abs(imag. part) = 1.
        /// Not referenced if JOBVL = &#39;N&#39;.
        /// </param>
        /// <param name="ldvl">
        /// [in] LDVL is INTEGER.
        /// The leading dimension of the matrix VL. LDVL &gt;= 1, and
        /// if JOBVL = &#39;V&#39;, LDVL &gt;= N.
        /// </param>
        /// <param name="vr">
        /// [out] VR is COMPLEX*16 array, dimension (LDVR,N).
        /// If JOBVR = &#39;V&#39;, the right generalized eigenvectors v(j) are
        /// stored one after another in the columns of VR, in the same
        /// order as their eigenvalues.
        /// Each eigenvector will be scaled so the largest component
        /// will have abs(real part) + abs(imag. part) = 1.
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
        /// If SENSE = &#39;N&#39; or &#39;V&#39;, RCONDE is not referenced.
        /// </param>
        /// <param name="rcondv">
        /// [out] RCONDV is DOUBLE PRECISION array, dimension (N).
        /// If JOB = &#39;V&#39; or &#39;B&#39;, the estimated reciprocal condition
        /// numbers of the eigenvectors, stored in consecutive elements
        /// of the array. If the eigenvalues cannot be reordered to
        /// compute RCONDV(j), RCONDV(j) is set to 0; this can only occur
        /// when the true value would be very small anyway.
        /// If SENSE = &#39;N&#39; or &#39;E&#39;, RCONDV is not referenced.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value.
        /// = 1,...,N:
        /// The QZ iteration failed.  No eigenvectors have been
        /// calculated, but ALPHA(j) and BETA(j) should be correct
        /// for j=INFO+1,...,N.
        /// &gt; N:  =N+1: other than QZ iteration failed in ZHGEQZ.
        /// =N+2: error return from ZTGEVC.
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
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zggevx", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zggevx(
            MatrixLayout matrixLayout,
            char balanc,
            char jobvl,
            char jobvr,
            char sense,
            int n,
            Complex* a,
            int lda,
            Complex* b,
            int ldb,
            Complex* alpha,
            Complex* beta,
            Complex* vl,
            int ldvl,
            Complex* vr,
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
