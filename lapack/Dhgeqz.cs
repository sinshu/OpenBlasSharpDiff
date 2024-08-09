using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DHGEQZ computes the eigenvalues of a real matrix pair (H,T),
        /// where H is an upper Hessenberg matrix and T is upper triangular,
        /// using the double-shift QZ method.
        /// Matrix pairs of this type are produced by the reduction to
        /// generalized upper Hessenberg form of a real matrix pair (A,B):
        /// </para>
        /// <para>
        ///    A = Q1*H*Z1**T,  B = Q1*T*Z1**T,
        /// </para>
        /// <para>
        /// as computed by DGGHRD.
        /// </para>
        /// <para>
        /// If JOB=&#39;S&#39;, then the Hessenberg-triangular pair (H,T) is
        /// also reduced to generalized Schur form,
        /// </para>
        /// <para>
        ///    H = Q*S*Z**T,  T = Q*P*Z**T,
        /// </para>
        /// <para>
        /// where Q and Z are orthogonal matrices, P is an upper triangular
        /// matrix, and S is a quasi-triangular matrix with 1-by-1 and 2-by-2
        /// diagonal blocks.
        /// </para>
        /// <para>
        /// The 1-by-1 blocks correspond to real eigenvalues of the matrix pair
        /// (H,T) and the 2-by-2 blocks correspond to complex conjugate pairs of
        /// eigenvalues.
        /// </para>
        /// <para>
        /// Additionally, the 2-by-2 upper triangular diagonal blocks of P
        /// corresponding to 2-by-2 blocks of S are reduced to positive diagonal
        /// form, i.e., if S(j+1,j) is non-zero, then P(j+1,j) = P(j,j+1) = 0,
        /// P(j,j) &gt; 0, and P(j+1,j+1) &gt; 0.
        /// </para>
        /// <para>
        /// Optionally, the orthogonal matrix Q from the generalized Schur
        /// factorization may be postmultiplied into an input matrix Q1, and the
        /// orthogonal matrix Z may be postmultiplied into an input matrix Z1.
        /// If Q1 and Z1 are the orthogonal matrices from DGGHRD that reduced
        /// the matrix pair (A,B) to generalized upper Hessenberg form, then the
        /// output matrices Q1*Q and Z1*Z are the orthogonal factors from the
        /// generalized Schur factorization of (A,B):
        /// </para>
        /// <para>
        ///    A = (Q1*Q)*S*(Z1*Z)**T,  B = (Q1*Q)*P*(Z1*Z)**T.
        /// </para>
        /// <para>
        /// To avoid overflow, eigenvalues of the matrix pair (H,T) (equivalently,
        /// of (A,B)) are computed as a pair of values (alpha,beta), where alpha is
        /// complex and beta real.
        /// If beta is nonzero, lambda = alpha / beta is an eigenvalue of the
        /// generalized nonsymmetric eigenvalue problem (GNEP)
        ///    A*x = lambda*B*x
        /// and if alpha is nonzero, mu = beta / alpha is an eigenvalue of the
        /// alternate form of the GNEP
        ///    mu*A*y = B*y.
        /// Real eigenvalues can be read directly from the generalized Schur
        /// form:
        ///   alpha = S(i,i), beta = P(i,i).
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
        /// = &#39;S&#39;: Compute eigenvalues and the Schur form.
        /// </param>
        /// <param name="compq">
        /// [in] COMPQ is CHARACTER*1.
        /// = &#39;N&#39;: Left Schur vectors (Q) are not computed;
        /// = &#39;I&#39;: Q is initialized to the unit matrix and the matrix Q
        /// of left Schur vectors of (H,T) is returned;
        /// = &#39;V&#39;: Q must contain an orthogonal matrix Q1 on entry and
        /// the product Q1*Q is returned.
        /// </param>
        /// <param name="compz">
        /// [in] COMPZ is CHARACTER*1.
        /// = &#39;N&#39;: Right Schur vectors (Z) are not computed;
        /// = &#39;I&#39;: Z is initialized to the unit matrix and the matrix Z
        /// of right Schur vectors of (H,T) is returned;
        /// = &#39;V&#39;: Z must contain an orthogonal matrix Z1 on entry and
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
        /// [in,out] H is DOUBLE PRECISION array, dimension (LDH, N).
        /// On entry, the N-by-N upper Hessenberg matrix H.
        /// On exit, if JOB = &#39;S&#39;, H contains the upper quasi-triangular
        /// matrix S from the generalized Schur factorization.
        /// If JOB = &#39;E&#39;, the diagonal blocks of H match those of S, but
        /// the rest of H is unspecified.
        /// </param>
        /// <param name="ldh">
        /// [in] LDH is INTEGER.
        /// The leading dimension of the array H.  LDH &gt;= max( 1, N ).
        /// </param>
        /// <param name="t">
        /// [in,out] T is DOUBLE PRECISION array, dimension (LDT, N).
        /// On entry, the N-by-N upper triangular matrix T.
        /// On exit, if JOB = &#39;S&#39;, T contains the upper triangular
        /// matrix P from the generalized Schur factorization;
        /// 2-by-2 diagonal blocks of P corresponding to 2-by-2 blocks of S
        /// are reduced to positive diagonal form, i.e., if H(j+1,j) is
        /// non-zero, then T(j+1,j) = T(j,j+1) = 0, T(j,j) &gt; 0, and
        /// T(j+1,j+1) &gt; 0.
        /// If JOB = &#39;E&#39;, the diagonal blocks of T match those of P, but
        /// the rest of T is unspecified.
        /// </param>
        /// <param name="ldt">
        /// [in] LDT is INTEGER.
        /// The leading dimension of the array T.  LDT &gt;= max( 1, N ).
        /// </param>
        /// <param name="alphar">
        /// [out] ALPHAR is DOUBLE PRECISION array, dimension (N).
        /// The real parts of each scalar alpha defining an eigenvalue
        /// of GNEP.
        /// </param>
        /// <param name="alphai">
        /// [out] ALPHAI is DOUBLE PRECISION array, dimension (N).
        /// The imaginary parts of each scalar alpha defining an
        /// eigenvalue of GNEP.
        /// If ALPHAI(j) is zero, then the j-th eigenvalue is real; if
        /// positive, then the j-th and (j+1)-st eigenvalues are a
        /// complex conjugate pair, with ALPHAI(j+1) = -ALPHAI(j).
        /// </param>
        /// <param name="beta">
        /// [out] BETA is DOUBLE PRECISION array, dimension (N).
        /// The scalars beta that define the eigenvalues of GNEP.
        /// Together, the quantities alpha = (ALPHAR(j),ALPHAI(j)) and
        /// beta = BETA(j) represent the j-th eigenvalue of the matrix
        /// pair (A,B), in one of the forms lambda = alpha/beta or
        /// mu = beta/alpha.  Since either lambda or mu may overflow,
        /// they should not, in general, be computed.
        /// </param>
        /// <param name="q">
        /// [in,out] Q is DOUBLE PRECISION array, dimension (LDQ, N).
        /// On entry, if COMPQ = &#39;V&#39;, the orthogonal matrix Q1 used in
        /// the reduction of (A,B) to generalized Hessenberg form.
        /// On exit, if COMPQ = &#39;I&#39;, the orthogonal matrix of left Schur
        /// vectors of (H,T), and if COMPQ = &#39;V&#39;, the orthogonal matrix
        /// of left Schur vectors of (A,B).
        /// Not referenced if COMPQ = &#39;N&#39;.
        /// </param>
        /// <param name="ldq">
        /// [in] LDQ is INTEGER.
        /// The leading dimension of the array Q.  LDQ &gt;= 1.
        /// If COMPQ=&#39;V&#39; or &#39;I&#39;, then LDQ &gt;= N.
        /// </param>
        /// <param name="z">
        /// [in,out] Z is DOUBLE PRECISION array, dimension (LDZ, N).
        /// On entry, if COMPZ = &#39;V&#39;, the orthogonal matrix Z1 used in
        /// the reduction of (A,B) to generalized Hessenberg form.
        /// On exit, if COMPZ = &#39;I&#39;, the orthogonal matrix of
        /// right Schur vectors of (H,T), and if COMPZ = &#39;V&#39;, the
        /// orthogonal matrix of right Schur vectors of (A,B).
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
        /// in Schur form, but ALPHAR(i), ALPHAI(i), and
        /// BETA(i), i=INFO+1,...,N should be correct.
        /// = N+1,...,2*N: the shift calculation failed.  (H,T) is not
        /// in Schur form, but ALPHAR(i), ALPHAI(i), and
        /// BETA(i), i=INFO-N+1,...,N should be correct.
        /// </returns>
        /// <remarks>
        /// <para>
        ///  Iteration counters:
        /// </para>
        /// <para>
        ///  JITER  -- counts iterations.
        ///  IITER  -- counts iterations run since ILAST was last
        ///            changed.  This is therefore reset only when a 1-by-1 or
        ///            2-by-2 block deflates off the bottom.
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dhgeqz", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dhgeqz(
            MatrixLayout matrixLayout,
            char job,
            char compq,
            char compz,
            int n,
            int ilo,
            int ihi,
            double* h,
            int ldh,
            double* t,
            int ldt,
            double* alphar,
            double* alphai,
            double* beta,
            double* q,
            int ldq,
            double* z,
            int ldz);
    }
}
