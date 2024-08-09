using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DGGSVD3 computes the generalized singular value decomposition (GSVD)
        /// of an M-by-N real matrix A and P-by-N real matrix B:
        /// </para>
        /// <para>
        ///       U**T*A*Q = D1*( 0 R ),    V**T*B*Q = D2*( 0 R )
        /// </para>
        /// <para>
        /// where U, V and Q are orthogonal matrices.
        /// Let K+L = the effective numerical rank of the matrix (A**T,B**T)**T,
        /// then R is a K+L-by-K+L nonsingular upper triangular matrix, D1 and
        /// D2 are M-by-(K+L) and P-by-(K+L) &quot;diagonal&quot; matrices and of the
        /// following structures, respectively:
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
        ///                   K  L
        ///        D2 =   L ( 0  S )
        ///             P-L ( 0  0 )
        /// </para>
        /// <para>
        ///                 N-K-L  K    L
        ///   ( 0 R ) = K (  0   R11  R12 )
        ///             L (  0    0   R22 )
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
        ///                   K M-K K+L-M
        ///        D1 =   K ( I  0    0   )
        ///             M-K ( 0  C    0   )
        /// </para>
        /// <para>
        ///                     K M-K K+L-M
        ///        D2 =   M-K ( 0  S    0  )
        ///             K+L-M ( 0  0    I  )
        ///               P-L ( 0  0    0  )
        /// </para>
        /// <para>
        ///                    N-K-L  K   M-K  K+L-M
        ///   ( 0 R ) =     K ( 0    R11  R12  R13  )
        ///               M-K ( 0     0   R22  R23  )
        ///             K+L-M ( 0     0    0   R33  )
        /// </para>
        /// <para>
        /// where
        /// </para>
        /// <para>
        ///   C = diag( ALPHA(K+1), ... , ALPHA(M) ),
        ///   S = diag( BETA(K+1),  ... , BETA(M) ),
        ///   C**2 + S**2 = I.
        /// </para>
        /// <para>
        ///   (R11 R12 R13 ) is stored in A(1:M, N-K-L+1:N), and R33 is stored
        ///   ( 0  R22 R23 )
        ///   in B(M-K+1:L,N+M-K-L+1:N) on exit.
        /// </para>
        /// <para>
        /// The routine computes C, S, R, and optionally the orthogonal
        /// transformation matrices U, V and Q.
        /// </para>
        /// <para>
        /// In particular, if B is an N-by-N nonsingular matrix, then the GSVD of
        /// A and B implicitly gives the SVD of A*inv(B):
        ///                      A*inv(B) = U*(D1*inv(D2))*V**T.
        /// If ( A**T,B**T)**T  has orthonormal columns, then the GSVD of A and B is
        /// also equal to the CS decomposition of A and B. Furthermore, the GSVD
        /// can be used to derive the solution of the eigenvalue problem:
        ///                      A**T*A x = lambda* B**T*B x.
        /// In some literature, the GSVD of A and B is presented in the form
        ///                  U**T*A*X = ( 0 D1 ),   V**T*B*X = ( 0 D2 )
        /// where U and V are orthogonal and X is nonsingular, D1 and D2 are
        /// ``diagonal&#39;&#39;.  The former GSVD form can be converted to the latter
        /// form by taking the nonsingular matrix X as
        /// </para>
        /// <para>
        ///                      X = Q*( I   0    )
        ///                            ( 0 inv(R) ).
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="jobu">
        /// [in] JOBU is CHARACTER*1.
        /// = &#39;U&#39;:  Orthogonal matrix U is computed;
        /// = &#39;N&#39;:  U is not computed.
        /// </param>
        /// <param name="jobv">
        /// [in] JOBV is CHARACTER*1.
        /// = &#39;V&#39;:  Orthogonal matrix V is computed;
        /// = &#39;N&#39;:  V is not computed.
        /// </param>
        /// <param name="jobq">
        /// [in] JOBQ is CHARACTER*1.
        /// = &#39;Q&#39;:  Orthogonal matrix Q is computed;
        /// = &#39;N&#39;:  Q is not computed.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of rows of the matrix A.  M &gt;= 0.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of columns of the matrices A and B.  N &gt;= 0.
        /// </param>
        /// <param name="p">
        /// [in] P is INTEGER.
        /// The number of rows of the matrix B.  P &gt;= 0.
        /// </param>
        /// <param name="k">
        /// [out] K is INTEGER.
        /// </param>
        /// <param name="l">
        /// [out] L is INTEGER.
        /// 
        /// On exit, K and L specify the dimension of the subblocks
        /// described in Purpose.
        /// K + L = effective numerical rank of (A**T,B**T)**T.
        /// </param>
        /// <param name="a">
        /// [in,out] A is DOUBLE PRECISION array, dimension (LDA,N).
        /// On entry, the M-by-N matrix A.
        /// On exit, A contains the triangular matrix R, or part of R.
        /// See Purpose for details.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A. LDA &gt;= max(1,M).
        /// </param>
        /// <param name="b">
        /// [in,out] B is DOUBLE PRECISION array, dimension (LDB,N).
        /// On entry, the P-by-N matrix B.
        /// On exit, B contains the triangular matrix R if M-K-L &lt; 0.
        /// See Purpose for details.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B. LDB &gt;= max(1,P).
        /// </param>
        /// <param name="alpha">
        /// [out] ALPHA is DOUBLE PRECISION array, dimension (N).
        /// </param>
        /// <param name="beta">
        /// [out] BETA is DOUBLE PRECISION array, dimension (N).
        /// 
        /// On exit, ALPHA and BETA contain the generalized singular
        /// value pairs of A and B;
        /// ALPHA(1:K) = 1,
        /// BETA(1:K)  = 0,
        /// and if M-K-L &gt;= 0,
        /// ALPHA(K+1:K+L) = C,
        /// BETA(K+1:K+L)  = S,
        /// or if M-K-L &lt; 0,
        /// ALPHA(K+1:M)=C, ALPHA(M+1:K+L)=0
        /// BETA(K+1:M) =S, BETA(M+1:K+L) =1
        /// and
        /// ALPHA(K+L+1:N) = 0
        /// BETA(K+L+1:N)  = 0
        /// </param>
        /// <param name="u">
        /// [out] U is DOUBLE PRECISION array, dimension (LDU,M).
        /// If JOBU = &#39;U&#39;, U contains the M-by-M orthogonal matrix U.
        /// If JOBU = &#39;N&#39;, U is not referenced.
        /// </param>
        /// <param name="ldu">
        /// [in] LDU is INTEGER.
        /// The leading dimension of the array U. LDU &gt;= max(1,M) if
        /// JOBU = &#39;U&#39;; LDU &gt;= 1 otherwise.
        /// </param>
        /// <param name="v">
        /// [out] V is DOUBLE PRECISION array, dimension (LDV,P).
        /// If JOBV = &#39;V&#39;, V contains the P-by-P orthogonal matrix V.
        /// If JOBV = &#39;N&#39;, V is not referenced.
        /// </param>
        /// <param name="ldv">
        /// [in] LDV is INTEGER.
        /// The leading dimension of the array V. LDV &gt;= max(1,P) if
        /// JOBV = &#39;V&#39;; LDV &gt;= 1 otherwise.
        /// </param>
        /// <param name="q">
        /// [out] Q is DOUBLE PRECISION array, dimension (LDQ,N).
        /// If JOBQ = &#39;Q&#39;, Q contains the N-by-N orthogonal matrix Q.
        /// If JOBQ = &#39;N&#39;, Q is not referenced.
        /// </param>
        /// <param name="ldq">
        /// [in] LDQ is INTEGER.
        /// The leading dimension of the array Q. LDQ &gt;= max(1,N) if
        /// JOBQ = &#39;Q&#39;; LDQ &gt;= 1 otherwise.
        /// </param>
        /// <param name="iwork">
        /// [out] IWORK is INTEGER array, dimension (N).
        /// On exit, IWORK stores the sorting information. More
        /// precisely, the following loop will sort ALPHA
        /// for I = K+1, min(M,K+L)
        /// swap ALPHA(I) and ALPHA(IWORK(I))
        /// endfor
        /// such that ALPHA(1) &gt;= ALPHA(2) &gt;= ... &gt;= ALPHA(N).
        /// </param>
        /// <returns>
        /// = 0:  successful exit.
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value.
        /// &gt; 0:  if INFO = 1, the Jacobi-type procedure failed to
        /// converge.  For further details, see subroutine DTGSJA.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dggsvd3", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dggsvd3(
            MatrixLayout matrixLayout,
            char jobu,
            char jobv,
            char jobq,
            int m,
            int n,
            int p,
            int* k,
            int* l,
            double* a,
            int lda,
            double* b,
            int ldb,
            double* alpha,
            double* beta,
            double* u,
            int ldu,
            double* v,
            int ldv,
            double* q,
            int ldq,
            int* iwork);
    }
}
