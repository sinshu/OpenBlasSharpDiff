using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CHGEQZ computes the eigenvalues of a complex matrix pair (H,T),
        /// where H is an upper Hessenberg matrix and T is upper triangular,
        /// using the single-shift QZ method.
        /// Matrix pairs of this type are produced by the reduction to
        /// generalized upper Hessenberg form of a complex matrix pair (A,B):
        /// </para>
        /// <para>
        ///    A = Q1*H*Z1**H,  B = Q1*T*Z1**H,
        /// </para>
        /// <para>
        /// as computed by CGGHRD.
        /// </para>
        /// <para>
        /// If JOB=&#39;S&#39;, then the Hessenberg-triangular pair (H,T) is
        /// also reduced to generalized Schur form,
        /// </para>
        /// <para>
        ///    H = Q*S*Z**H,  T = Q*P*Z**H,
        /// </para>
        /// <para>
        /// where Q and Z are unitary matrices and S and P are upper triangular.
        /// </para>
        /// <para>
        /// Optionally, the unitary matrix Q from the generalized Schur
        /// factorization may be postmultiplied into an input matrix Q1, and the
        /// unitary matrix Z may be postmultiplied into an input matrix Z1.
        /// If Q1 and Z1 are the unitary matrices from CGGHRD that reduced
        /// the matrix pair (A,B) to generalized Hessenberg form, then the output
        /// matrices Q1*Q and Z1*Z are the unitary factors from the generalized
        /// Schur factorization of (A,B):
        /// </para>
        /// <para>
        ///    A = (Q1*Q)*S*(Z1*Z)**H,  B = (Q1*Q)*P*(Z1*Z)**H.
        /// </para>
        /// <para>
        /// To avoid overflow, eigenvalues of the matrix pair (H,T)
        /// (equivalently, of (A,B)) are computed as a pair of complex values
        /// (alpha,beta).  If beta is nonzero, lambda = alpha / beta is an
        /// eigenvalue of the generalized nonsymmetric eigenvalue problem (GNEP)
        ///    A*x = lambda*B*x
        /// and if alpha is nonzero, mu = beta / alpha is an eigenvalue of the
        /// alternate form of the GNEP
        ///    mu*A*y = B*y.
        /// The values of alpha and beta for the i-th eigenvalue can be read
        /// directly from the generalized Schur form:  alpha = S(i,i),
        /// beta = P(i,i).
        /// </para>
        /// <para>
        /// Ref: C.B. Moler &amp; G.W. Stewart, &quot;An Algorithm for Generalized Matrix
        ///      Eigenvalue Problems&quot;, SIAM J. Numer. Anal., 10(1973),
        ///      pp. 241--256.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="job">
        /// [in] JOB is CHARACTER*1.
        /// = &#39;E&#39;: Compute eigenvalues only;
        /// = &#39;S&#39;: Computer eigenvalues and the Schur form.
        /// </param>
        /// <param name="compq">
        /// [in] COMPQ is CHARACTER*1.
        /// = &#39;N&#39;: Left Schur vectors (Q) are not computed;
        /// = &#39;I&#39;: Q is initialized to the unit matrix and the matrix Q
        /// of left Schur vectors of (H,T) is returned;
        /// = &#39;V&#39;: Q must contain a unitary matrix Q1 on entry and
        /// the product Q1*Q is returned.
        /// </param>
        /// <param name="compz">
        /// [in] COMPZ is CHARACTER*1.
        /// = &#39;N&#39;: Right Schur vectors (Z) are not computed;
        /// = &#39;I&#39;: Q is initialized to the unit matrix and the matrix Z
        /// of right Schur vectors of (H,T) is returned;
        /// = &#39;V&#39;: Z must contain a unitary matrix Z1 on entry and
        /// the product Z1*Z is returned.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrices H, T, Q, and Z.  N &gt;= 0.
        /// </param>
        /// <param name="ilo">
        /// [in] ILO is INTEGER.
        /// </param>
        /// <param name="ihi">
        /// [in] IHI is INTEGER.
        /// ILO and IHI mark the rows and columns of H which are in
        /// Hessenberg form.  It is assumed that A is already upper
        /// triangular in rows and columns 1:ILO-1 and IHI+1:N.
        /// If N &gt; 0, 1 &lt;= ILO &lt;= IHI &lt;= N; if N = 0, ILO=1 and IHI=0.
        /// </param>
        /// <param name="h">
        /// [in,out] H is COMPLEX array, dimension (LDH, N).
        /// On entry, the N-by-N upper Hessenberg matrix H.
        /// On exit, if JOB = &#39;S&#39;, H contains the upper triangular
        /// matrix S from the generalized Schur factorization.
        /// If JOB = &#39;E&#39;, the diagonal of H matches that of S, but
        /// the rest of H is unspecified.
        /// </param>
        /// <param name="ldh">
        /// [in] LDH is INTEGER.
        /// The leading dimension of the array H.  LDH &gt;= max( 1, N ).
        /// </param>
        /// <param name="t">
        /// [in,out] T is COMPLEX array, dimension (LDT, N).
        /// On entry, the N-by-N upper triangular matrix T.
        /// On exit, if JOB = &#39;S&#39;, T contains the upper triangular
        /// matrix P from the generalized Schur factorization.
        /// If JOB = &#39;E&#39;, the diagonal of T matches that of P, but
        /// the rest of T is unspecified.
        /// </param>
        /// <param name="ldt">
        /// [in] LDT is INTEGER.
        /// The leading dimension of the array T.  LDT &gt;= max( 1, N ).
        /// </param>
        /// <param name="alpha">
        /// [out] ALPHA is COMPLEX array, dimension (N).
        /// The complex scalars alpha that define the eigenvalues of
        /// GNEP.  ALPHA(i) = S(i,i) in the generalized Schur
        /// factorization.
        /// </param>
        /// <param name="beta">
        /// [out] BETA is COMPLEX array, dimension (N).
        /// The real non-negative scalars beta that define the
        /// eigenvalues of GNEP.  BETA(i) = P(i,i) in the generalized
        /// Schur factorization.
        /// 
        /// Together, the quantities alpha = ALPHA(j) and beta = BETA(j)
        /// represent the j-th eigenvalue of the matrix pair (A,B), in
        /// one of the forms lambda = alpha/beta or mu = beta/alpha.
        /// Since either lambda or mu may overflow, they should not,
        /// in general, be computed.
        /// </param>
        /// <param name="q">
        /// [in,out] Q is COMPLEX array, dimension (LDQ, N).
        /// On entry, if COMPQ = &#39;V&#39;, the unitary matrix Q1 used in the
        /// reduction of (A,B) to generalized Hessenberg form.
        /// On exit, if COMPQ = &#39;I&#39;, the unitary matrix of left Schur
        /// vectors of (H,T), and if COMPQ = &#39;V&#39;, the unitary matrix of
        /// left Schur vectors of (A,B).
        /// Not referenced if COMPQ = &#39;N&#39;.
        /// </param>
        /// <param name="ldq">
        /// [in] LDQ is INTEGER.
        /// The leading dimension of the array Q.  LDQ &gt;= 1.
        /// If COMPQ=&#39;V&#39; or &#39;I&#39;, then LDQ &gt;= N.
        /// </param>
        /// <param name="z">
        /// [in,out] Z is COMPLEX array, dimension (LDZ, N).
        /// On entry, if COMPZ = &#39;V&#39;, the unitary matrix Z1 used in the
        /// reduction of (A,B) to generalized Hessenberg form.
        /// On exit, if COMPZ = &#39;I&#39;, the unitary matrix of right Schur
        /// vectors of (H,T), and if COMPZ = &#39;V&#39;, the unitary matrix of
        /// right Schur vectors of (A,B).
        /// Not referenced if COMPZ = &#39;N&#39;.
        /// </param>
        /// <param name="ldz">
        /// [in] LDZ is INTEGER.
        /// The leading dimension of the array Z.  LDZ &gt;= 1.
        /// If COMPZ=&#39;V&#39; or &#39;I&#39;, then LDZ &gt;= N.
        /// </param>
        /// <returns>
        /// = 0: successful exit
        /// &lt; 0: if INFO = -i, the i-th argument had an illegal value
        /// = 1,...,N: the QZ iteration did not converge.  (H,T) is not
        /// in Schur form, but ALPHA(i) and BETA(i),
        /// i=INFO+1,...,N should be correct.
        /// = N+1,...,2*N: the shift calculation failed.  (H,T) is not
        /// in Schur form, but ALPHA(i) and BETA(i),
        /// i=INFO-N+1,...,N should be correct.
        /// </returns>
        /// <remarks>
        /// <para>
        ///  We assume that complex ABS works as long as its value is less than
        ///  overflow.
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_chgeqz", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Chgeqz(
            MatrixLayout matrixLayout,
            char job,
            char compq,
            char compz,
            int n,
            int ilo,
            int ihi,
            Complex32* h,
            int ldh,
            Complex32* t,
            int ldt,
            Complex32* alpha,
            Complex32* beta,
            Complex32* q,
            int ldq,
            Complex32* z,
            int ldz);
    }
}
