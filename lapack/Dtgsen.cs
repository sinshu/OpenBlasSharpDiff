using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DTGSEN reorders the generalized real Schur decomposition of a real
        /// matrix pair (A, B) (in terms of an orthonormal equivalence trans-
        /// formation Q**T * (A, B) * Z), so that a selected cluster of eigenvalues
        /// appears in the leading diagonal blocks of the upper quasi-triangular
        /// matrix A and the upper triangular B. The leading columns of Q and
        /// Z form orthonormal bases of the corresponding left and right eigen-
        /// spaces (deflating subspaces). (A, B) must be in generalized real
        /// Schur canonical form (as returned by DGGES), i.e. A is block upper
        /// triangular with 1-by-1 and 2-by-2 diagonal blocks. B is upper
        /// triangular.
        /// </para>
        /// <para>
        /// DTGSEN also computes the generalized eigenvalues
        /// </para>
        /// <para>
        ///             w(j) = (ALPHAR(j) + i*ALPHAI(j))/BETA(j)
        /// </para>
        /// <para>
        /// of the reordered matrix pair (A, B).
        /// </para>
        /// <para>
        /// Optionally, DTGSEN computes the estimates of reciprocal condition
        /// numbers for eigenvalues and eigenspaces. These are Difu[(A11,B11),
        /// (A22,B22)] and Difl[(A11,B11), (A22,B22)], i.e. the separation(s)
        /// between the matrix pairs (A11, B11) and (A22,B22) that correspond to
        /// the selected cluster and the eigenvalues outside the cluster, resp.,
        /// and norms of &quot;projections&quot; onto left and right eigenspaces w.r.t.
        /// the selected cluster in the (1,1)-block.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="ijob">
        /// [in] IJOB is INTEGER.
        /// Specifies whether condition numbers are required for the
        /// cluster of eigenvalues (PL and PR) or the deflating subspaces
        /// (Difu and Difl):
        /// =0: Only reorder w.r.t. SELECT. No extras.
        /// =1: Reciprocal of norms of &quot;projections&quot; onto left and right
        /// eigenspaces w.r.t. the selected cluster (PL and PR).
        /// =2: Upper bounds on Difu and Difl. F-norm-based estimate
        /// (DIF(1:2)).
        /// =3: Estimate of Difu and Difl. 1-norm-based estimate
        /// (DIF(1:2)).
        /// About 5 times as expensive as IJOB = 2.
        /// =4: Compute PL, PR and DIF (i.e. 0, 1 and 2 above): Economic
        /// version to get it all.
        /// =5: Compute PL, PR and DIF (i.e. 0, 1 and 3 above)
        /// </param>
        /// <param name="wantq">
        /// [in] WANTQ is LOGICAL.
        /// .TRUE. : update the left transformation matrix Q;
        /// .FALSE.: do not update Q.
        /// </param>
        /// <param name="wantz">
        /// [in] WANTZ is LOGICAL.
        /// .TRUE. : update the right transformation matrix Z;
        /// .FALSE.: do not update Z.
        /// </param>
        /// <param name="select">
        /// [in] SELECT is LOGICAL array, dimension (N).
        /// SELECT specifies the eigenvalues in the selected cluster.
        /// To select a real eigenvalue w(j), SELECT(j) must be set to
        /// .TRUE.. To select a complex conjugate pair of eigenvalues
        /// w(j) and w(j+1), corresponding to a 2-by-2 diagonal block,
        /// either SELECT(j) or SELECT(j+1) or both must be set to
        /// .TRUE.; a complex conjugate pair of eigenvalues must be
        /// either both included in the cluster or both excluded.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrices A and B. N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is DOUBLE PRECISION array, dimension(LDA,N).
        /// On entry, the upper quasi-triangular matrix A, with (A, B) in
        /// generalized real Schur canonical form.
        /// On exit, A is overwritten by the reordered matrix A.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A. LDA &gt;= max(1,N).
        /// </param>
        /// <param name="b">
        /// [in,out] B is DOUBLE PRECISION array, dimension(LDB,N).
        /// On entry, the upper triangular matrix B, with (A, B) in
        /// generalized real Schur canonical form.
        /// On exit, B is overwritten by the reordered matrix B.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B. LDB &gt;= max(1,N).
        /// </param>
        /// <param name="alphar">
        /// [out] ALPHAR is DOUBLE PRECISION array, dimension (N).
        /// </param>
        /// <param name="alphai">
        /// [out] ALPHAI is DOUBLE PRECISION array, dimension (N).
        /// </param>
        /// <param name="beta">
        /// [out] BETA is DOUBLE PRECISION array, dimension (N).
        /// 
        /// On exit, (ALPHAR(j) + ALPHAI(j)*i)/BETA(j), j=1,...,N, will
        /// be the generalized eigenvalues.  ALPHAR(j) + ALPHAI(j)*i
        /// and BETA(j),j=1,...,N  are the diagonals of the complex Schur
        /// form (S,T) that would result if the 2-by-2 diagonal blocks of
        /// the real generalized Schur form of (A,B) were further reduced
        /// to triangular form using complex unitary transformations.
        /// If ALPHAI(j) is zero, then the j-th eigenvalue is real; if
        /// positive, then the j-th and (j+1)-st eigenvalues are a
        /// complex conjugate pair, with ALPHAI(j+1) negative.
        /// </param>
        /// <param name="q">
        /// [in,out] Q is DOUBLE PRECISION array, dimension (LDQ,N).
        /// On entry, if WANTQ = .TRUE., Q is an N-by-N matrix.
        /// On exit, Q has been postmultiplied by the left orthogonal
        /// transformation matrix which reorder (A, B); The leading M
        /// columns of Q form orthonormal bases for the specified pair of
        /// left eigenspaces (deflating subspaces).
        /// If WANTQ = .FALSE., Q is not referenced.
        /// </param>
        /// <param name="ldq">
        /// [in] LDQ is INTEGER.
        /// The leading dimension of the array Q.  LDQ &gt;= 1;
        /// and if WANTQ = .TRUE., LDQ &gt;= N.
        /// </param>
        /// <param name="z">
        /// [in,out] Z is DOUBLE PRECISION array, dimension (LDZ,N).
        /// On entry, if WANTZ = .TRUE., Z is an N-by-N matrix.
        /// On exit, Z has been postmultiplied by the left orthogonal
        /// transformation matrix which reorder (A, B); The leading M
        /// columns of Z form orthonormal bases for the specified pair of
        /// left eigenspaces (deflating subspaces).
        /// If WANTZ = .FALSE., Z is not referenced.
        /// </param>
        /// <param name="ldz">
        /// [in] LDZ is INTEGER.
        /// The leading dimension of the array Z. LDZ &gt;= 1;
        /// If WANTZ = .TRUE., LDZ &gt;= N.
        /// </param>
        /// <param name="m">
        /// [out] M is INTEGER.
        /// The dimension of the specified pair of left and right eigen-
        /// spaces (deflating subspaces). 0 &lt;= M &lt;= N.
        /// </param>
        /// <param name="pl">
        /// [out] PL is DOUBLE PRECISION.
        /// </param>
        /// <param name="pr">
        /// [out] PR is DOUBLE PRECISION.
        /// 
        /// If IJOB = 1, 4 or 5, PL, PR are lower bounds on the
        /// reciprocal of the norm of &quot;projections&quot; onto left and right
        /// eigenspaces with respect to the selected cluster.
        /// 0 &lt; PL, PR &lt;= 1.
        /// If M = 0 or M = N, PL = PR  = 1.
        /// If IJOB = 0, 2 or 3, PL and PR are not referenced.
        /// </param>
        /// <param name="dif">
        /// [out] DIF is DOUBLE PRECISION array, dimension (2).
        /// If IJOB &gt;= 2, DIF(1:2) store the estimates of Difu and Difl.
        /// If IJOB = 2 or 4, DIF(1:2) are F-norm-based upper bounds on
        /// Difu and Difl. If IJOB = 3 or 5, DIF(1:2) are 1-norm-based
        /// estimates of Difu and Difl.
        /// If M = 0 or N, DIF(1:2) = F-norm([A, B]).
        /// If IJOB = 0 or 1, DIF is not referenced.
        /// </param>
        /// <returns>
        /// =0: Successful exit.
        /// &lt;0: If INFO = -i, the i-th argument had an illegal value.
        /// =1: Reordering of (A, B) failed because the transformed
        /// matrix pair (A, B) would be too far from generalized
        /// Schur form; the problem is very ill-conditioned.
        /// (A, B) may have been partially reordered.
        /// If requested, 0 is returned in DIF(*), PL and PR.
        /// </returns>
        /// <remarks>
        /// <para>
        ///  DTGSEN first collects the selected eigenvalues by computing
        ///  orthogonal U and W that move them to the top left corner of (A, B).
        ///  In other words, the selected eigenvalues are the eigenvalues of
        ///  (A11, B11) in:
        /// </para>
        /// <para>
        ///              U**T*(A, B)*W = (A11 A12) (B11 B12) n1
        ///                              ( 0  A22),( 0  B22) n2
        ///                                n1  n2    n1  n2
        /// </para>
        /// <para>
        ///  where N = n1+n2 and U**T means the transpose of U. The first n1 columns
        ///  of U and W span the specified pair of left and right eigenspaces
        ///  (deflating subspaces) of (A, B).
        /// </para>
        /// <para>
        ///  If (A, B) has been obtained from the generalized real Schur
        ///  decomposition of a matrix pair (C, D) = Q*(A, B)*Z**T, then the
        ///  reordered generalized real Schur form of (C, D) is given by
        /// </para>
        /// <para>
        ///           (C, D) = (Q*U)*(U**T*(A, B)*W)*(Z*W)**T,
        /// </para>
        /// <para>
        ///  and the first n1 columns of Q*U and Z*W span the corresponding
        ///  deflating subspaces of (C, D) (Q and Z store Q*U and Z*W, resp.).
        /// </para>
        /// <para>
        ///  Note that if the selected eigenvalue is sufficiently ill-conditioned,
        ///  then its value may differ significantly from its value before
        ///  reordering.
        /// </para>
        /// <para>
        ///  The reciprocal condition numbers of the left and right eigenspaces
        ///  spanned by the first n1 columns of U and W (or Q*U and Z*W) may
        ///  be returned in DIF(1:2), corresponding to Difu and Difl, resp.
        /// </para>
        /// <para>
        ///  The Difu and Difl are defined as:
        /// </para>
        /// <para>
        ///       Difu[(A11, B11), (A22, B22)] = sigma-min( Zu )
        ///  and
        ///       Difl[(A11, B11), (A22, B22)] = Difu[(A22, B22), (A11, B11)],
        /// </para>
        /// <para>
        ///  where sigma-min(Zu) is the smallest singular value of the
        ///  (2*n1*n2)-by-(2*n1*n2) matrix
        /// </para>
        /// <para>
        ///       Zu = [ kron(In2, A11)  -kron(A22**T, In1) ]
        ///            [ kron(In2, B11)  -kron(B22**T, In1) ].
        /// </para>
        /// <para>
        ///  Here, Inx is the identity matrix of size nx and A22**T is the
        ///  transpose of A22. kron(X, Y) is the Kronecker product between
        ///  the matrices X and Y.
        /// </para>
        /// <para>
        ///  When DIF(2) is small, small changes in (A, B) can cause large changes
        ///  in the deflating subspace. An approximate (asymptotic) bound on the
        ///  maximum angular error in the computed deflating subspaces is
        /// </para>
        /// <para>
        ///       EPS * norm((A, B)) / DIF(2),
        /// </para>
        /// <para>
        ///  where EPS is the machine precision.
        /// </para>
        /// <para>
        ///  The reciprocal norm of the projectors on the left and right
        ///  eigenspaces associated with (A11, B11) may be returned in PL and PR.
        ///  They are computed as follows. First we compute L and R so that
        ///  P*(A, B)*Q is block diagonal, where
        /// </para>
        /// <para>
        ///       P = ( I -L ) n1           Q = ( I R ) n1
        ///           ( 0  I ) n2    and        ( 0 I ) n2
        ///             n1 n2                    n1 n2
        /// </para>
        /// <para>
        ///  and (L, R) is the solution to the generalized Sylvester equation
        /// </para>
        /// <para>
        ///       A11*R - L*A22 = -A12
        ///       B11*R - L*B22 = -B12
        /// </para>
        /// <para>
        ///  Then PL = (F-norm(L)**2+1)**(-1/2) and PR = (F-norm(R)**2+1)**(-1/2).
        ///  An approximate (asymptotic) bound on the average absolute error of
        ///  the selected eigenvalues is
        /// </para>
        /// <para>
        ///       EPS * norm((A, B)) / PL.
        /// </para>
        /// <para>
        ///  There are also global error bounds which valid for perturbations up
        ///  to a certain restriction:  A lower bound (x) on the smallest
        ///  F-norm(E,F) for which an eigenvalue of (A11, B11) may move and
        ///  coalesce with an eigenvalue of (A22, B22) under perturbation (E,F),
        ///  (i.e. (A + E, B + F), is
        /// </para>
        /// <para>
        ///   x = min(Difu,Difl)/((1/(PL*PL)+1/(PR*PR))**(1/2)+2*max(1/PL,1/PR)).
        /// </para>
        /// <para>
        ///  An approximate bound on x can be computed from DIF(1:2), PL and PR.
        /// </para>
        /// <para>
        ///  If y = ( F-norm(E,F) / x) &lt;= 1, the angles between the perturbed
        ///  (L&#39;, R&#39;) and unperturbed (L, R) left and right deflating subspaces
        ///  associated with the selected cluster in the (1,1)-blocks can be
        ///  bounded as
        /// </para>
        /// <para>
        ///   max-angle(L, L&#39;) &lt;= arctan( y * PL / (1 - y * (1 - PL * PL)**(1/2))
        ///   max-angle(R, R&#39;) &lt;= arctan( y * PR / (1 - y * (1 - PR * PR)**(1/2))
        /// </para>
        /// <para>
        ///  See LAPACK User&#39;s Guide section 4.11 or the following references
        ///  for more information.
        /// </para>
        /// <para>
        ///  Note that if the default method for computing the Frobenius-norm-
        ///  based estimate DIF is not wanted (see DLATDF), then the parameter
        ///  IDIFJB (see below) should be changed from 3 to 4 (routine DLATDF
        ///  (IJOB = 2 will be used)). See DTGSYL for more details.
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dtgsen", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dtgsen(
            MatrixLayout matrixLayout,
            int ijob,
            bool wantq,
            bool wantz,
            bool* select,
            int n,
            double* a,
            int lda,
            double* b,
            int ldb,
            double* alphar,
            double* alphai,
            double* beta,
            double* q,
            int ldq,
            double* z,
            int ldz,
            int* m,
            double* pl,
            double* pr,
            double* dif);
    }
}
