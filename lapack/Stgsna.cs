using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// STGSNA estimates reciprocal condition numbers for specified
        /// eigenvalues and/or eigenvectors of a matrix pair (A, B) in
        /// generalized real Schur canonical form (or of any matrix pair
        /// (Q*A*Z**T, Q*B*Z**T) with orthogonal matrices Q and Z, where
        /// Z**T denotes the transpose of Z.
        /// </para>
        /// <para>
        /// (A, B) must be in generalized real Schur form (as returned by SGGES),
        /// i.e. A is block upper triangular with 1-by-1 and 2-by-2 diagonal
        /// blocks. B is upper triangular.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="job">
        /// [in] JOB is CHARACTER*1.
        /// Specifies whether condition numbers are required for
        /// eigenvalues (S) or eigenvectors (DIF):
        /// = &#39;E&#39;: for eigenvalues only (S);
        /// = &#39;V&#39;: for eigenvectors only (DIF);
        /// = &#39;B&#39;: for both eigenvalues and eigenvectors (S and DIF).
        /// </param>
        /// <param name="howmny">
        /// [in] HOWMNY is CHARACTER*1.
        /// = &#39;A&#39;: compute condition numbers for all eigenpairs;
        /// = &#39;S&#39;: compute condition numbers for selected eigenpairs
        /// specified by the array SELECT.
        /// </param>
        /// <param name="select">
        /// [in] SELECT is LOGICAL array, dimension (N).
        /// If HOWMNY = &#39;S&#39;, SELECT specifies the eigenpairs for which
        /// condition numbers are required. To select condition numbers
        /// for the eigenpair corresponding to a real eigenvalue w(j),
        /// SELECT(j) must be set to .TRUE.. To select condition numbers
        /// corresponding to a complex conjugate pair of eigenvalues w(j)
        /// and w(j+1), either SELECT(j) or SELECT(j+1) or both, must be
        /// set to .TRUE..
        /// If HOWMNY = &#39;A&#39;, SELECT is not referenced.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the square matrix pair (A, B). N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in] A is REAL array, dimension (LDA,N).
        /// The upper quasi-triangular matrix A in the pair (A,B).
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A. LDA &gt;= max(1,N).
        /// </param>
        /// <param name="b">
        /// [in] B is REAL array, dimension (LDB,N).
        /// The upper triangular matrix B in the pair (A,B).
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B. LDB &gt;= max(1,N).
        /// </param>
        /// <param name="vl">
        /// [in] VL is REAL array, dimension (LDVL,M).
        /// If JOB = &#39;E&#39; or &#39;B&#39;, VL must contain left eigenvectors of
        /// (A, B), corresponding to the eigenpairs specified by HOWMNY
        /// and SELECT. The eigenvectors must be stored in consecutive
        /// columns of VL, as returned by STGEVC.
        /// If JOB = &#39;V&#39;, VL is not referenced.
        /// </param>
        /// <param name="ldvl">
        /// [in] LDVL is INTEGER.
        /// The leading dimension of the array VL. LDVL &gt;= 1.
        /// If JOB = &#39;E&#39; or &#39;B&#39;, LDVL &gt;= N.
        /// </param>
        /// <param name="vr">
        /// [in] VR is REAL array, dimension (LDVR,M).
        /// If JOB = &#39;E&#39; or &#39;B&#39;, VR must contain right eigenvectors of
        /// (A, B), corresponding to the eigenpairs specified by HOWMNY
        /// and SELECT. The eigenvectors must be stored in consecutive
        /// columns ov VR, as returned by STGEVC.
        /// If JOB = &#39;V&#39;, VR is not referenced.
        /// </param>
        /// <param name="ldvr">
        /// [in] LDVR is INTEGER.
        /// The leading dimension of the array VR. LDVR &gt;= 1.
        /// If JOB = &#39;E&#39; or &#39;B&#39;, LDVR &gt;= N.
        /// </param>
        /// <param name="s">
        /// [out] S is REAL array, dimension (MM).
        /// If JOB = &#39;E&#39; or &#39;B&#39;, the reciprocal condition numbers of the
        /// selected eigenvalues, stored in consecutive elements of the
        /// array. For a complex conjugate pair of eigenvalues two
        /// consecutive elements of S are set to the same value. Thus
        /// S(j), DIF(j), and the j-th columns of VL and VR all
        /// correspond to the same eigenpair (but not in general the
        /// j-th eigenpair, unless all eigenpairs are selected).
        /// If JOB = &#39;V&#39;, S is not referenced.
        /// </param>
        /// <param name="dif">
        /// [out] DIF is REAL array, dimension (MM).
        /// If JOB = &#39;V&#39; or &#39;B&#39;, the estimated reciprocal condition
        /// numbers of the selected eigenvectors, stored in consecutive
        /// elements of the array. For a complex eigenvector two
        /// consecutive elements of DIF are set to the same value. If
        /// the eigenvalues cannot be reordered to compute DIF(j), DIF(j)
        /// is set to 0; this can only occur when the true value would be
        /// very small anyway.
        /// If JOB = &#39;E&#39;, DIF is not referenced.
        /// </param>
        /// <param name="mm">
        /// [in] MM is INTEGER.
        /// The number of elements in the arrays S and DIF. MM &gt;= M.
        /// </param>
        /// <param name="m">
        /// [out] M is INTEGER.
        /// The number of elements of the arrays S and DIF used to store
        /// the specified condition numbers; for each selected real
        /// eigenvalue one element is used, and for each selected complex
        /// conjugate pair of eigenvalues, two elements are used.
        /// If HOWMNY = &#39;A&#39;, M is set to N.
        /// </param>
        /// <returns>
        /// =0: Successful exit
        /// &lt;0: If INFO = -i, the i-th argument had an illegal value
        /// </returns>
        /// <remarks>
        /// <para>
        ///  The reciprocal of the condition number of a generalized eigenvalue
        ///  w = (a, b) is defined as
        /// </para>
        /// <para>
        ///       S(w) = (|u**TAv|**2 + |u**TBv|**2)**(1/2) / (norm(u)*norm(v))
        /// </para>
        /// <para>
        ///  where u and v are the left and right eigenvectors of (A, B)
        ///  corresponding to w; |z| denotes the absolute value of the complex
        ///  number, and norm(u) denotes the 2-norm of the vector u.
        ///  The pair (a, b) corresponds to an eigenvalue w = a/b (= u**TAv/u**TBv)
        ///  of the matrix pair (A, B). If both a and b equal zero, then (A B) is
        ///  singular and S(I) = -1 is returned.
        /// </para>
        /// <para>
        ///  An approximate error bound on the chordal distance between the i-th
        ///  computed generalized eigenvalue w and the corresponding exact
        ///  eigenvalue lambda is
        /// </para>
        /// <para>
        ///       chord(w, lambda) &lt;= EPS * norm(A, B) / S(I)
        /// </para>
        /// <para>
        ///  where EPS is the machine precision.
        /// </para>
        /// <para>
        ///  The reciprocal of the condition number DIF(i) of right eigenvector u
        ///  and left eigenvector v corresponding to the generalized eigenvalue w
        ///  is defined as follows:
        /// </para>
        /// <para>
        ///  a) If the i-th eigenvalue w = (a,b) is real
        /// </para>
        /// <para>
        ///     Suppose U and V are orthogonal transformations such that
        /// </para>
        /// <para>
        ///              U**T*(A, B)*V  = (S, T) = ( a   *  ) ( b  *  )  1
        ///                                        ( 0  S22 ),( 0 T22 )  n-1
        ///                                          1  n-1     1 n-1
        /// </para>
        /// <para>
        ///     Then the reciprocal condition number DIF(i) is
        /// </para>
        /// <para>
        ///                Difl((a, b), (S22, T22)) = sigma-min( Zl ),
        /// </para>
        /// <para>
        ///     where sigma-min(Zl) denotes the smallest singular value of the
        ///     2(n-1)-by-2(n-1) matrix
        /// </para>
        /// <para>
        ///         Zl = [ kron(a, In-1)  -kron(1, S22) ]
        ///              [ kron(b, In-1)  -kron(1, T22) ] .
        /// </para>
        /// <para>
        ///     Here In-1 is the identity matrix of size n-1. kron(X, Y) is the
        ///     Kronecker product between the matrices X and Y.
        /// </para>
        /// <para>
        ///     Note that if the default method for computing DIF(i) is wanted
        ///     (see SLATDF), then the parameter DIFDRI (see below) should be
        ///     changed from 3 to 4 (routine SLATDF(IJOB = 2 will be used)).
        ///     See STGSYL for more details.
        /// </para>
        /// <para>
        ///  b) If the i-th and (i+1)-th eigenvalues are complex conjugate pair,
        /// </para>
        /// <para>
        ///     Suppose U and V are orthogonal transformations such that
        /// </para>
        /// <para>
        ///              U**T*(A, B)*V = (S, T) = ( S11  *   ) ( T11  *  )  2
        ///                                       ( 0    S22 ),( 0    T22) n-2
        ///                                         2    n-2     2    n-2
        /// </para>
        /// <para>
        ///     and (S11, T11) corresponds to the complex conjugate eigenvalue
        ///     pair (w, conjg(w)). There exist unitary matrices U1 and V1 such
        ///     that
        /// </para>
        /// <para>
        ///       U1**T*S11*V1 = ( s11 s12 ) and U1**T*T11*V1 = ( t11 t12 )
        ///                      (  0  s22 )                    (  0  t22 )
        /// </para>
        /// <para>
        ///     where the generalized eigenvalues w = s11/t11 and
        ///     conjg(w) = s22/t22.
        /// </para>
        /// <para>
        ///     Then the reciprocal condition number DIF(i) is bounded by
        /// </para>
        /// <para>
        ///         min( d1, max( 1, |real(s11)/real(s22)| )*d2 )
        /// </para>
        /// <para>
        ///     where, d1 = Difl((s11, t11), (s22, t22)) = sigma-min(Z1), where
        ///     Z1 is the complex 2-by-2 matrix
        /// </para>
        /// <para>
        ///              Z1 =  [ s11  -s22 ]
        ///                    [ t11  -t22 ],
        /// </para>
        /// <para>
        ///     This is done by computing (using real arithmetic) the
        ///     roots of the characteristical polynomial det(Z1**T * Z1 - lambda I),
        ///     where Z1**T denotes the transpose of Z1 and det(X) denotes
        ///     the determinant of X.
        /// </para>
        /// <para>
        ///     and d2 is an upper bound on Difl((S11, T11), (S22, T22)), i.e. an
        ///     upper bound on sigma-min(Z2), where Z2 is (2n-2)-by-(2n-2)
        /// </para>
        /// <para>
        ///              Z2 = [ kron(S11**T, In-2)  -kron(I2, S22) ]
        ///                   [ kron(T11**T, In-2)  -kron(I2, T22) ]
        /// </para>
        /// <para>
        ///     Note that if the default method for computing DIF is wanted (see
        ///     SLATDF), then the parameter DIFDRI (see below) should be changed
        ///     from 3 to 4 (routine SLATDF(IJOB = 2 will be used)). See STGSYL
        ///     for more details.
        /// </para>
        /// <para>
        ///  For each eigenvalue/vector specified by SELECT, DIF stores a
        ///  Frobenius norm-based estimate of Difl.
        /// </para>
        /// <para>
        ///  An approximate error bound for the i-th computed eigenvector VL(i) or
        ///  VR(i) is given by
        /// </para>
        /// <para>
        ///             EPS * norm(A, B) / DIF(i).
        /// </para>
        /// <para>
        ///  See ref. [2-3] for more details and further references.
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_stgsna", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Stgsna(
            MatrixLayout matrixLayout,
            char job,
            char howmny,
            bool* select,
            int n,
            float* a,
            int lda,
            float* b,
            int ldb,
            float* vl,
            int ldvl,
            float* vr,
            int ldvr,
            float* s,
            float* dif,
            int mm,
            int* m);
    }
}
