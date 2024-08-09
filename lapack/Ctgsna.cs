using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CTGSNA estimates reciprocal condition numbers for specified
        /// eigenvalues and/or eigenvectors of a matrix pair (A, B).
        /// </para>
        /// <para>
        /// (A, B) must be in generalized Schur canonical form, that is, A and
        /// B are both upper triangular.
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
        /// for the corresponding j-th eigenvalue and/or eigenvector,
        /// SELECT(j) must be set to .TRUE..
        /// If HOWMNY = &#39;A&#39;, SELECT is not referenced.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the square matrix pair (A, B). N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in] A is COMPLEX array, dimension (LDA,N).
        /// The upper triangular matrix A in the pair (A,B).
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A. LDA &gt;= max(1,N).
        /// </param>
        /// <param name="b">
        /// [in] B is COMPLEX array, dimension (LDB,N).
        /// The upper triangular matrix B in the pair (A, B).
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B. LDB &gt;= max(1,N).
        /// </param>
        /// <param name="vl">
        /// [in] VL is COMPLEX array, dimension (LDVL,M).
        /// IF JOB = &#39;E&#39; or &#39;B&#39;, VL must contain left eigenvectors of
        /// (A, B), corresponding to the eigenpairs specified by HOWMNY
        /// and SELECT.  The eigenvectors must be stored in consecutive
        /// columns of VL, as returned by CTGEVC.
        /// If JOB = &#39;V&#39;, VL is not referenced.
        /// </param>
        /// <param name="ldvl">
        /// [in] LDVL is INTEGER.
        /// The leading dimension of the array VL. LDVL &gt;= 1; and
        /// If JOB = &#39;E&#39; or &#39;B&#39;, LDVL &gt;= N.
        /// </param>
        /// <param name="vr">
        /// [in] VR is COMPLEX array, dimension (LDVR,M).
        /// IF JOB = &#39;E&#39; or &#39;B&#39;, VR must contain right eigenvectors of
        /// (A, B), corresponding to the eigenpairs specified by HOWMNY
        /// and SELECT.  The eigenvectors must be stored in consecutive
        /// columns of VR, as returned by CTGEVC.
        /// If JOB = &#39;V&#39;, VR is not referenced.
        /// </param>
        /// <param name="ldvr">
        /// [in] LDVR is INTEGER.
        /// The leading dimension of the array VR. LDVR &gt;= 1;
        /// If JOB = &#39;E&#39; or &#39;B&#39;, LDVR &gt;= N.
        /// </param>
        /// <param name="s">
        /// [out] S is REAL array, dimension (MM).
        /// If JOB = &#39;E&#39; or &#39;B&#39;, the reciprocal condition numbers of the
        /// selected eigenvalues, stored in consecutive elements of the
        /// array.
        /// If JOB = &#39;V&#39;, S is not referenced.
        /// </param>
        /// <param name="dif">
        /// [out] DIF is REAL array, dimension (MM).
        /// If JOB = &#39;V&#39; or &#39;B&#39;, the estimated reciprocal condition
        /// numbers of the selected eigenvectors, stored in consecutive
        /// elements of the array.
        /// If the eigenvalues cannot be reordered to compute DIF(j),
        /// DIF(j) is set to 0; this can only occur when the true value
        /// would be very small anyway.
        /// For each eigenvalue/vector specified by SELECT, DIF stores
        /// a Frobenius norm-based estimate of Difl.
        /// If JOB = &#39;E&#39;, DIF is not referenced.
        /// </param>
        /// <param name="mm">
        /// [in] MM is INTEGER.
        /// The number of elements in the arrays S and DIF. MM &gt;= M.
        /// </param>
        /// <param name="m">
        /// [out] M is INTEGER.
        /// The number of elements of the arrays S and DIF used to store
        /// the specified condition numbers; for each selected eigenvalue
        /// one element is used. If HOWMNY = &#39;A&#39;, M is set to N.
        /// </param>
        /// <returns>
        /// = 0: Successful exit
        /// &lt; 0: If INFO = -i, the i-th argument had an illegal value
        /// </returns>
        /// <remarks>
        /// <para>
        ///  The reciprocal of the condition number of the i-th generalized
        ///  eigenvalue w = (a, b) is defined as
        /// </para>
        /// <para>
        ///          S(I) = (|v**HAu|**2 + |v**HBu|**2)**(1/2) / (norm(u)*norm(v))
        /// </para>
        /// <para>
        ///  where u and v are the right and left eigenvectors of (A, B)
        ///  corresponding to w; |z| denotes the absolute value of the complex
        ///  number, and norm(u) denotes the 2-norm of the vector u. The pair
        ///  (a, b) corresponds to an eigenvalue w = a/b (= v**HAu/v**HBu) of the
        ///  matrix pair (A, B). If both a and b equal zero, then (A,B) is
        ///  singular and S(I) = -1 is returned.
        /// </para>
        /// <para>
        ///  An approximate error bound on the chordal distance between the i-th
        ///  computed generalized eigenvalue w and the corresponding exact
        ///  eigenvalue lambda is
        /// </para>
        /// <para>
        ///          chord(w, lambda) &lt;=   EPS * norm(A, B) / S(I),
        /// </para>
        /// <para>
        ///  where EPS is the machine precision.
        /// </para>
        /// <para>
        ///  The reciprocal of the condition number of the right eigenvector u
        ///  and left eigenvector v corresponding to the generalized eigenvalue w
        ///  is defined as follows. Suppose
        /// </para>
        /// <para>
        ///                   (A, B) = ( a   *  ) ( b  *  )  1
        ///                            ( 0  A22 ),( 0 B22 )  n-1
        ///                              1  n-1     1 n-1
        /// </para>
        /// <para>
        ///  Then the reciprocal condition number DIF(I) is
        /// </para>
        /// <para>
        ///          Difl[(a, b), (A22, B22)]  = sigma-min( Zl )
        /// </para>
        /// <para>
        ///  where sigma-min(Zl) denotes the smallest singular value of
        /// </para>
        /// <para>
        ///         Zl = [ kron(a, In-1) -kron(1, A22) ]
        ///              [ kron(b, In-1) -kron(1, B22) ].
        /// </para>
        /// <para>
        ///  Here In-1 is the identity matrix of size n-1 and X**H is the conjugate
        ///  transpose of X. kron(X, Y) is the Kronecker product between the
        ///  matrices X and Y.
        /// </para>
        /// <para>
        ///  We approximate the smallest singular value of Zl with an upper
        ///  bound. This is done by CLATDF.
        /// </para>
        /// <para>
        ///  An approximate error bound for a computed eigenvector VL(i) or
        ///  VR(i) is given by
        /// </para>
        /// <para>
        ///                      EPS * norm(A, B) / DIF(i).
        /// </para>
        /// <para>
        ///  See ref. [2-3] for more details and further references.
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_ctgsna", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Ctgsna(
            MatrixLayout matrixLayout,
            char job,
            char howmny,
            bool* select,
            int n,
            Complex32* a,
            int lda,
            Complex32* b,
            int ldb,
            Complex32* vl,
            int ldvl,
            Complex32* vr,
            int ldvr,
            float* s,
            float* dif,
            int mm,
            int* m);
    }
}
