using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DTRSEN reorders the real Schur factorization of a real matrix
        /// A = Q*T*Q**T, so that a selected cluster of eigenvalues appears in
        /// the leading diagonal blocks of the upper quasi-triangular matrix T,
        /// and the leading columns of Q form an orthonormal basis of the
        /// corresponding right invariant subspace.
        /// </para>
        /// <para>
        /// Optionally the routine computes the reciprocal condition numbers of
        /// the cluster of eigenvalues and/or the invariant subspace.
        /// </para>
        /// <para>
        /// T must be in Schur canonical form (as returned by DHSEQR), that is,
        /// block upper triangular with 1-by-1 and 2-by-2 diagonal blocks; each
        /// 2-by-2 diagonal block has its diagonal elements equal and its
        /// off-diagonal elements of opposite sign.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="job">
        /// [in] JOB is CHARACTER*1.
        /// Specifies whether condition numbers are required for the
        /// cluster of eigenvalues (S) or the invariant subspace (SEP):
        /// = &#39;N&#39;: none;
        /// = &#39;E&#39;: for eigenvalues only (S);
        /// = &#39;V&#39;: for invariant subspace only (SEP);
        /// = &#39;B&#39;: for both eigenvalues and invariant subspace (S and
        /// SEP).
        /// </param>
        /// <param name="compq">
        /// [in] COMPQ is CHARACTER*1.
        /// = &#39;V&#39;: update the matrix Q of Schur vectors;
        /// = &#39;N&#39;: do not update Q.
        /// </param>
        /// <param name="select">
        /// [in] SELECT is LOGICAL array, dimension (N).
        /// SELECT specifies the eigenvalues in the selected cluster. To
        /// select a real eigenvalue w(j), SELECT(j) must be set to
        /// .TRUE.. To select a complex conjugate pair of eigenvalues
        /// w(j) and w(j+1), corresponding to a 2-by-2 diagonal block,
        /// either SELECT(j) or SELECT(j+1) or both must be set to
        /// .TRUE.; a complex conjugate pair of eigenvalues must be
        /// either both included in the cluster or both excluded.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix T. N &gt;= 0.
        /// </param>
        /// <param name="t">
        /// [in,out] T is DOUBLE PRECISION array, dimension (LDT,N).
        /// On entry, the upper quasi-triangular matrix T, in Schur
        /// canonical form.
        /// On exit, T is overwritten by the reordered matrix T, again in
        /// Schur canonical form, with the selected eigenvalues in the
        /// leading diagonal blocks.
        /// </param>
        /// <param name="ldt">
        /// [in] LDT is INTEGER.
        /// The leading dimension of the array T. LDT &gt;= max(1,N).
        /// </param>
        /// <param name="q">
        /// [in,out] Q is DOUBLE PRECISION array, dimension (LDQ,N).
        /// On entry, if COMPQ = &#39;V&#39;, the matrix Q of Schur vectors.
        /// On exit, if COMPQ = &#39;V&#39;, Q has been postmultiplied by the
        /// orthogonal transformation matrix which reorders T; the
        /// leading M columns of Q form an orthonormal basis for the
        /// specified invariant subspace.
        /// If COMPQ = &#39;N&#39;, Q is not referenced.
        /// </param>
        /// <param name="ldq">
        /// [in] LDQ is INTEGER.
        /// The leading dimension of the array Q.
        /// LDQ &gt;= 1; and if COMPQ = &#39;V&#39;, LDQ &gt;= N.
        /// </param>
        /// <param name="wr">
        /// [out] WR is DOUBLE PRECISION array, dimension (N).
        /// </param>
        /// <param name="wi">
        /// [out] WI is DOUBLE PRECISION array, dimension (N).
        /// 
        /// The real and imaginary parts, respectively, of the reordered
        /// eigenvalues of T. The eigenvalues are stored in the same
        /// order as on the diagonal of T, with WR(i) = T(i,i) and, if
        /// T(i:i+1,i:i+1) is a 2-by-2 diagonal block, WI(i) &gt; 0 and
        /// WI(i+1) = -WI(i). Note that if a complex eigenvalue is
        /// sufficiently ill-conditioned, then its value may differ
        /// significantly from its value before reordering.
        /// </param>
        /// <param name="m">
        /// [out] M is INTEGER.
        /// The dimension of the specified invariant subspace.
        /// 0 &lt; = M &lt;= N.
        /// </param>
        /// <param name="s">
        /// [out] S is DOUBLE PRECISION.
        /// If JOB = &#39;E&#39; or &#39;B&#39;, S is a lower bound on the reciprocal
        /// condition number for the selected cluster of eigenvalues.
        /// S cannot underestimate the true reciprocal condition number
        /// by more than a factor of sqrt(N). If M = 0 or N, S = 1.
        /// If JOB = &#39;N&#39; or &#39;V&#39;, S is not referenced.
        /// </param>
        /// <param name="sep">
        /// [out] SEP is DOUBLE PRECISION.
        /// If JOB = &#39;V&#39; or &#39;B&#39;, SEP is the estimated reciprocal
        /// condition number of the specified invariant subspace. If
        /// M = 0 or N, SEP = norm(T).
        /// If JOB = &#39;N&#39; or &#39;E&#39;, SEP is not referenced.
        /// </param>
        /// <returns>
        /// = 0: successful exit
        /// &lt; 0: if INFO = -i, the i-th argument had an illegal value
        /// = 1: reordering of T failed because some eigenvalues are too
        /// close to separate (the problem is very ill-conditioned);
        /// T may have been partially reordered, and WR and WI
        /// contain the eigenvalues in the same order as in T; S and
        /// SEP (if requested) are set to zero.
        /// </returns>
        /// <remarks>
        /// <para>
        ///  DTRSEN first collects the selected eigenvalues by computing an
        ///  orthogonal transformation Z to move them to the top left corner of T.
        ///  In other words, the selected eigenvalues are the eigenvalues of T11
        ///  in:
        /// </para>
        /// <para>
        ///          Z**T * T * Z = ( T11 T12 ) n1
        ///                         (  0  T22 ) n2
        ///                            n1  n2
        /// </para>
        /// <para>
        ///  where N = n1+n2 and Z**T means the transpose of Z. The first n1 columns
        ///  of Z span the specified invariant subspace of T.
        /// </para>
        /// <para>
        ///  If T has been obtained from the real Schur factorization of a matrix
        ///  A = Q*T*Q**T, then the reordered real Schur factorization of A is given
        ///  by A = (Q*Z)*(Z**T*T*Z)*(Q*Z)**T, and the first n1 columns of Q*Z span
        ///  the corresponding invariant subspace of A.
        /// </para>
        /// <para>
        ///  The reciprocal condition number of the average of the eigenvalues of
        ///  T11 may be returned in S. S lies between 0 (very badly conditioned)
        ///  and 1 (very well conditioned). It is computed as follows. First we
        ///  compute R so that
        /// </para>
        /// <para>
        ///                         P = ( I  R ) n1
        ///                             ( 0  0 ) n2
        ///                               n1 n2
        /// </para>
        /// <para>
        ///  is the projector on the invariant subspace associated with T11.
        ///  R is the solution of the Sylvester equation:
        /// </para>
        /// <para>
        ///                        T11*R - R*T22 = T12.
        /// </para>
        /// <para>
        ///  Let F-norm(M) denote the Frobenius-norm of M and 2-norm(M) denote
        ///  the two-norm of M. Then S is computed as the lower bound
        /// </para>
        /// <para>
        ///                      (1 + F-norm(R)**2)**(-1/2)
        /// </para>
        /// <para>
        ///  on the reciprocal of 2-norm(P), the true reciprocal condition number.
        ///  S cannot underestimate 1 / 2-norm(P) by more than a factor of
        ///  sqrt(N).
        /// </para>
        /// <para>
        ///  An approximate error bound for the computed average of the
        ///  eigenvalues of T11 is
        /// </para>
        /// <para>
        ///                         EPS * norm(T) / S
        /// </para>
        /// <para>
        ///  where EPS is the machine precision.
        /// </para>
        /// <para>
        ///  The reciprocal condition number of the right invariant subspace
        ///  spanned by the first n1 columns of Z (or of Q*Z) is returned in SEP.
        ///  SEP is defined as the separation of T11 and T22:
        /// </para>
        /// <para>
        ///                     sep( T11, T22 ) = sigma-min( C )
        /// </para>
        /// <para>
        ///  where sigma-min(C) is the smallest singular value of the
        ///  n1*n2-by-n1*n2 matrix
        /// </para>
        /// <para>
        ///     C  = kprod( I(n2), T11 ) - kprod( transpose(T22), I(n1) )
        /// </para>
        /// <para>
        ///  I(m) is an m by m identity matrix, and kprod denotes the Kronecker
        ///  product. We estimate sigma-min(C) by the reciprocal of an estimate of
        ///  the 1-norm of inverse(C). The true reciprocal 1-norm of inverse(C)
        ///  cannot differ from sigma-min(C) by more than a factor of sqrt(n1*n2).
        /// </para>
        /// <para>
        ///  When SEP is small, small changes in T can cause large changes in
        ///  the invariant subspace. An approximate bound on the maximum angular
        ///  error in the computed right invariant subspace is
        /// </para>
        /// <para>
        ///                      EPS * norm(T) / SEP
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dtrsen", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dtrsen(
            MatrixLayout matrixLayout,
            char job,
            char compq,
            bool* select,
            int n,
            double* t,
            int ldt,
            double* q,
            int ldq,
            double* wr,
            double* wi,
            int* m,
            double* s,
            double* sep);
    }
}
