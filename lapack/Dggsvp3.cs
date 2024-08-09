﻿using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DGGSVP3 computes orthogonal matrices U, V and Q such that
        /// </para>
        /// <para>
        ///                    N-K-L  K    L
        ///  U**T*A*Q =     K ( 0    A12  A13 )  if M-K-L &gt;= 0;
        ///                 L ( 0     0   A23 )
        ///             M-K-L ( 0     0    0  )
        /// </para>
        /// <para>
        ///                  N-K-L  K    L
        ///         =     K ( 0    A12  A13 )  if M-K-L &lt; 0;
        ///             M-K ( 0     0   A23 )
        /// </para>
        /// <para>
        ///                  N-K-L  K    L
        ///  V**T*B*Q =   L ( 0     0   B13 )
        ///             P-L ( 0     0    0  )
        /// </para>
        /// <para>
        /// where the K-by-K matrix A12 and L-by-L matrix B13 are nonsingular
        /// upper triangular; A23 is L-by-L upper triangular if M-K-L &gt;= 0,
        /// otherwise A23 is (M-K)-by-L upper trapezoidal.  K+L = the effective
        /// numerical rank of the (M+P)-by-N matrix (A**T,B**T)**T.
        /// </para>
        /// <para>
        /// This decomposition is the preprocessing step for computing the
        /// Generalized Singular Value Decomposition (GSVD), see subroutine
        /// DGGSVD3.
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
        /// <param name="p">
        /// [in] P is INTEGER.
        /// The number of rows of the matrix B.  P &gt;= 0.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of columns of the matrices A and B.  N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is DOUBLE PRECISION array, dimension (LDA,N).
        /// On entry, the M-by-N matrix A.
        /// On exit, A contains the triangular (or trapezoidal) matrix
        /// described in the Purpose section.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A. LDA &gt;= max(1,M).
        /// </param>
        /// <param name="b">
        /// [in,out] B is DOUBLE PRECISION array, dimension (LDB,N).
        /// On entry, the P-by-N matrix B.
        /// On exit, B contains the triangular matrix described in
        /// the Purpose section.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B. LDB &gt;= max(1,P).
        /// </param>
        /// <param name="tola">
        /// [in] TOLA is DOUBLE PRECISION.
        /// </param>
        /// <param name="tolb">
        /// [in] TOLB is DOUBLE PRECISION.
        /// 
        /// TOLA and TOLB are the thresholds to determine the effective
        /// numerical rank of matrix B and a subblock of A. Generally,
        /// they are set to
        /// TOLA = MAX(M,N)*norm(A)*MACHEPS,
        /// TOLB = MAX(P,N)*norm(B)*MACHEPS.
        /// The size of TOLA and TOLB may affect the size of backward
        /// errors of the decomposition.
        /// </param>
        /// <param name="k">
        /// [out] K is INTEGER.
        /// </param>
        /// <param name="l">
        /// [out] L is INTEGER.
        /// 
        /// On exit, K and L specify the dimension of the subblocks
        /// described in Purpose section.
        /// K + L = effective numerical rank of (A**T,B**T)**T.
        /// </param>
        /// <param name="u">
        /// [out] U is DOUBLE PRECISION array, dimension (LDU,M).
        /// If JOBU = &#39;U&#39;, U contains the orthogonal matrix U.
        /// If JOBU = &#39;N&#39;, U is not referenced.
        /// </param>
        /// <param name="ldu">
        /// [in] LDU is INTEGER.
        /// The leading dimension of the array U. LDU &gt;= max(1,M) if
        /// JOBU = &#39;U&#39;; LDU &gt;= 1 otherwise.
        /// </param>
        /// <param name="v">
        /// [out] V is DOUBLE PRECISION array, dimension (LDV,P).
        /// If JOBV = &#39;V&#39;, V contains the orthogonal matrix V.
        /// If JOBV = &#39;N&#39;, V is not referenced.
        /// </param>
        /// <param name="ldv">
        /// [in] LDV is INTEGER.
        /// The leading dimension of the array V. LDV &gt;= max(1,P) if
        /// JOBV = &#39;V&#39;; LDV &gt;= 1 otherwise.
        /// </param>
        /// <param name="q">
        /// [out] Q is DOUBLE PRECISION array, dimension (LDQ,N).
        /// If JOBQ = &#39;Q&#39;, Q contains the orthogonal matrix Q.
        /// If JOBQ = &#39;N&#39;, Q is not referenced.
        /// </param>
        /// <param name="ldq">
        /// [in] LDQ is INTEGER.
        /// The leading dimension of the array Q. LDQ &gt;= max(1,N) if
        /// JOBQ = &#39;Q&#39;; LDQ &gt;= 1 otherwise.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value.
        /// </returns>
        /// <remarks>
        /// <para>
        ///  The subroutine uses LAPACK subroutine DGEQP3 for the QR factorization
        ///  with column pivoting to detect the effective numerical rank of the
        ///  a matrix. It may be replaced by a better rank determination strategy.
        /// </para>
        /// <para>
        ///  DGGSVP3 replaces the deprecated subroutine DGGSVP.
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dggsvp3", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dggsvp3(
            MatrixLayout matrixLayout,
            char jobu,
            char jobv,
            char jobq,
            int m,
            int p,
            int n,
            double* a,
            int lda,
            double* b,
            int ldb,
            double tola,
            double tolb,
            int* k,
            int* l,
            double* u,
            int ldu,
            double* v,
            int ldv,
            double* q,
            int ldq);
    }
}
