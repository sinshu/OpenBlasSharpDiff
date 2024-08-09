using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CTGSJA computes the generalized singular value decomposition (GSVD)
        /// of two complex upper triangular (or trapezoidal) matrices A and B.
        /// </para>
        /// <para>
        /// On entry, it is assumed that matrices A and B have the following
        /// forms, which may be obtained by the preprocessing subroutine CGGSVP
        /// from a general M-by-N matrix A and P-by-N matrix B:
        /// </para>
        /// <para>
        ///              N-K-L  K    L
        ///    A =    K ( 0    A12  A13 ) if M-K-L &gt;= 0;
        ///           L ( 0     0   A23 )
        ///       M-K-L ( 0     0    0  )
        /// </para>
        /// <para>
        ///            N-K-L  K    L
        ///    A =  K ( 0    A12  A13 ) if M-K-L &lt; 0;
        ///       M-K ( 0     0   A23 )
        /// </para>
        /// <para>
        ///            N-K-L  K    L
        ///    B =  L ( 0     0   B13 )
        ///       P-L ( 0     0    0  )
        /// </para>
        /// <para>
        /// where the K-by-K matrix A12 and L-by-L matrix B13 are nonsingular
        /// upper triangular; A23 is L-by-L upper triangular if M-K-L &gt;= 0,
        /// otherwise A23 is (M-K)-by-L upper trapezoidal.
        /// </para>
        /// <para>
        /// On exit,
        /// </para>
        /// <para>
        ///        U**H *A*Q = D1*( 0 R ),    V**H *B*Q = D2*( 0 R ),
        /// </para>
        /// <para>
        /// where U, V and Q are unitary matrices.
        /// R is a nonsingular upper triangular matrix, and D1
        /// and D2 are ``diagonal&#39;&#39; matrices, which are of the following
        /// structures:
        /// </para>
        /// <para>
        /// If M-K-L &gt;= 0,
        /// </para>
        /// <para>
        ///                     K  L
        ///        D1 =     K ( I  0 )
        ///                 L ( 0  C )
        ///             M-K-L ( 0  0 )
        /// </para>
        /// <para>
        ///                    K  L
        ///        D2 = L   ( 0  S )
        ///             P-L ( 0  0 )
        /// </para>
        /// <para>
        ///                N-K-L  K    L
        ///   ( 0 R ) = K (  0   R11  R12 ) K
        ///             L (  0    0   R22 ) L
        /// </para>
        /// <para>
        /// where
        /// </para>
        /// <para>
        ///   C = diag( ALPHA(K+1), ... , ALPHA(K+L) ),
        ///   S = diag( BETA(K+1),  ... , BETA(K+L) ),
        ///   C**2 + S**2 = I.
        /// </para>
        /// <para>
        ///   R is stored in A(1:K+L,N-K-L+1:N) on exit.
        /// </para>
        /// <para>
        /// If M-K-L &lt; 0,
        /// </para>
        /// <para>
        ///                K M-K K+L-M
        ///     D1 =   K ( I  0    0   )
        ///          M-K ( 0  C    0   )
        /// </para>
        /// <para>
        ///                  K M-K K+L-M
        ///     D2 =   M-K ( 0  S    0   )
        ///          K+L-M ( 0  0    I   )
        ///            P-L ( 0  0    0   )
        /// </para>
        /// <para>
        ///                N-K-L  K   M-K  K+L-M
        /// ( 0 R ) =    K ( 0    R11  R12  R13  )
        ///           M-K ( 0     0   R22  R23  )
        ///         K+L-M ( 0     0    0   R33  )
        /// </para>
        /// <para>
        /// where
        /// C = diag( ALPHA(K+1), ... , ALPHA(M) ),
        /// S = diag( BETA(K+1),  ... , BETA(M) ),
        /// C**2 + S**2 = I.
        /// </para>
        /// <para>
        /// R = ( R11 R12 R13 ) is stored in A(1:M, N-K-L+1:N) and R33 is stored
        ///     (  0  R22 R23 )
        /// in B(M-K+1:L,N+M-K-L+1:N) on exit.
        /// </para>
        /// <para>
        /// The computation of the unitary transformation matrices U, V or Q
        /// is optional.  These matrices may either be formed explicitly, or they
        /// may be postmultiplied into input matrices U1, V1, or Q1.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="jobu">
        /// [in] JOBU is CHARACTER*1.
        /// = &#39;U&#39;:  U must contain a unitary matrix U1 on entry, and
        /// the product U1*U is returned;
        /// = &#39;I&#39;:  U is initialized to the unit matrix, and the
        /// unitary matrix U is returned;
        /// = &#39;N&#39;:  U is not computed.
        /// </param>
        /// <param name="jobv">
        /// [in] JOBV is CHARACTER*1.
        /// = &#39;V&#39;:  V must contain a unitary matrix V1 on entry, and
        /// the product V1*V is returned;
        /// = &#39;I&#39;:  V is initialized to the unit matrix, and the
        /// unitary matrix V is returned;
        /// = &#39;N&#39;:  V is not computed.
        /// </param>
        /// <param name="jobq">
        /// [in] JOBQ is CHARACTER*1.
        /// = &#39;Q&#39;:  Q must contain a unitary matrix Q1 on entry, and
        /// the product Q1*Q is returned;
        /// = &#39;I&#39;:  Q is initialized to the unit matrix, and the
        /// unitary matrix Q is returned;
        /// = &#39;N&#39;:  Q is not computed.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of rows of the matrix A.  M &gt;= 0.
        /// </param>
        /// <param name="p">
        /// [in] P is INTEGER.
        /// The number of rows of the matrix B.  P &gt;= 0.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of columns of the matrices A and B.  N &gt;= 0.
        /// </param>
        /// <param name="k">
        /// [in] K is INTEGER.
        /// </param>
        /// <param name="l">
        /// [in] L is INTEGER.
        /// 
        /// K and L specify the subblocks in the input matrices A and B:
        /// A23 = A(K+1:MIN(K+L,M),N-L+1:N) and B13 = B(1:L,,N-L+1:N)
        /// of A and B, whose GSVD is going to be computed by CTGSJA.
        /// See Further Details.
        /// </param>
        /// <param name="a">
        /// [in,out] A is COMPLEX array, dimension (LDA,N).
        /// On entry, the M-by-N matrix A.
        /// On exit, A(N-K+1:N,1:MIN(K+L,M) ) contains the triangular
        /// matrix R or part of R.  See Purpose for details.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A. LDA &gt;= max(1,M).
        /// </param>
        /// <param name="b">
        /// [in,out] B is COMPLEX array, dimension (LDB,N).
        /// On entry, the P-by-N matrix B.
        /// On exit, if necessary, B(M-K+1:L,N+M-K-L+1:N) contains
        /// a part of R.  See Purpose for details.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B. LDB &gt;= max(1,P).
        /// </param>
        /// <param name="tola">
        /// [in] TOLA is REAL.
        /// </param>
        /// <param name="tolb">
        /// [in] TOLB is REAL.
        /// 
        /// TOLA and TOLB are the convergence criteria for the Jacobi-
        /// Kogbetliantz iteration procedure. Generally, they are the
        /// same as used in the preprocessing step, say
        /// TOLA = MAX(M,N)*norm(A)*MACHEPS,
        /// TOLB = MAX(P,N)*norm(B)*MACHEPS.
        /// </param>
        /// <param name="alpha">
        /// [out] ALPHA is REAL array, dimension (N).
        /// </param>
        /// <param name="beta">
        /// [out] BETA is REAL array, dimension (N).
        /// 
        /// On exit, ALPHA and BETA contain the generalized singular
        /// value pairs of A and B;
        /// ALPHA(1:K) = 1,
        /// BETA(1:K)  = 0,
        /// and if M-K-L &gt;= 0,
        /// ALPHA(K+1:K+L) = diag(C),
        /// BETA(K+1:K+L)  = diag(S),
        /// or if M-K-L &lt; 0,
        /// ALPHA(K+1:M)= C, ALPHA(M+1:K+L)= 0
        /// BETA(K+1:M) = S, BETA(M+1:K+L) = 1.
        /// Furthermore, if K+L &lt; N,
        /// ALPHA(K+L+1:N) = 0
        /// BETA(K+L+1:N)  = 0.
        /// </param>
        /// <param name="u">
        /// [in,out] U is COMPLEX array, dimension (LDU,M).
        /// On entry, if JOBU = &#39;U&#39;, U must contain a matrix U1 (usually
        /// the unitary matrix returned by CGGSVP).
        /// On exit,
        /// if JOBU = &#39;I&#39;, U contains the unitary matrix U;
        /// if JOBU = &#39;U&#39;, U contains the product U1*U.
        /// If JOBU = &#39;N&#39;, U is not referenced.
        /// </param>
        /// <param name="ldu">
        /// [in] LDU is INTEGER.
        /// The leading dimension of the array U. LDU &gt;= max(1,M) if
        /// JOBU = &#39;U&#39;; LDU &gt;= 1 otherwise.
        /// </param>
        /// <param name="v">
        /// [in,out] V is COMPLEX array, dimension (LDV,P).
        /// On entry, if JOBV = &#39;V&#39;, V must contain a matrix V1 (usually
        /// the unitary matrix returned by CGGSVP).
        /// On exit,
        /// if JOBV = &#39;I&#39;, V contains the unitary matrix V;
        /// if JOBV = &#39;V&#39;, V contains the product V1*V.
        /// If JOBV = &#39;N&#39;, V is not referenced.
        /// </param>
        /// <param name="ldv">
        /// [in] LDV is INTEGER.
        /// The leading dimension of the array V. LDV &gt;= max(1,P) if
        /// JOBV = &#39;V&#39;; LDV &gt;= 1 otherwise.
        /// </param>
        /// <param name="q">
        /// [in,out] Q is COMPLEX array, dimension (LDQ,N).
        /// On entry, if JOBQ = &#39;Q&#39;, Q must contain a matrix Q1 (usually
        /// the unitary matrix returned by CGGSVP).
        /// On exit,
        /// if JOBQ = &#39;I&#39;, Q contains the unitary matrix Q;
        /// if JOBQ = &#39;Q&#39;, Q contains the product Q1*Q.
        /// If JOBQ = &#39;N&#39;, Q is not referenced.
        /// </param>
        /// <param name="ldq">
        /// [in] LDQ is INTEGER.
        /// The leading dimension of the array Q. LDQ &gt;= max(1,N) if
        /// JOBQ = &#39;Q&#39;; LDQ &gt;= 1 otherwise.
        /// </param>
        /// <param name="ncycle">
        /// [out] NCYCLE is INTEGER.
        /// The number of cycles required for convergence.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value.
        /// = 1:  the procedure does not converge after MAXIT cycles.
        /// </returns>
        /// <remarks>
        /// <para>
        ///  CTGSJA essentially uses a variant of Kogbetliantz algorithm to reduce
        ///  min(L,M-K)-by-L triangular (or trapezoidal) matrix A23 and L-by-L
        ///  matrix B13 to the form:
        /// </para>
        /// <para>
        ///           U1**H *A13*Q1 = C1*R1; V1**H *B13*Q1 = S1*R1,
        /// </para>
        /// <para>
        ///  where U1, V1 and Q1 are unitary matrix.
        ///  C1 and S1 are diagonal matrices satisfying
        /// </para>
        /// <para>
        ///                C1**2 + S1**2 = I,
        /// </para>
        /// <para>
        ///  and R1 is an L-by-L nonsingular upper triangular matrix.
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_ctgsja", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Ctgsja(
            MatrixLayout matrixLayout,
            char jobu,
            char jobv,
            char jobq,
            int m,
            int p,
            int n,
            int k,
            int l,
            Complex32* a,
            int lda,
            Complex32* b,
            int ldb,
            float tola,
            float tolb,
            float* alpha,
            float* beta,
            Complex32* u,
            int ldu,
            Complex32* v,
            int ldv,
            Complex32* q,
            int ldq,
            int* ncycle);
    }
}
