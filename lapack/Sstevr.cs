using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// SSTEVR computes selected eigenvalues and, optionally, eigenvectors
        /// of a real symmetric tridiagonal matrix T.  Eigenvalues and
        /// eigenvectors can be selected by specifying either a range of values
        /// or a range of indices for the desired eigenvalues.
        /// </para>
        /// <para>
        /// Whenever possible, SSTEVR calls SSTEMR to compute the
        /// eigenspectrum using Relatively Robust Representations.  SSTEMR
        /// computes eigenvalues by the dqds algorithm, while orthogonal
        /// eigenvectors are computed from various &quot;good&quot; L D L^T representations
        /// (also known as Relatively Robust Representations). Gram-Schmidt
        /// orthogonalization is avoided as far as possible. More specifically,
        /// the various steps of the algorithm are as follows. For the i-th
        /// unreduced block of T,
        ///    (a) Compute T - sigma_i = L_i D_i L_i^T, such that L_i D_i L_i^T
        ///         is a relatively robust representation,
        ///    (b) Compute the eigenvalues, lambda_j, of L_i D_i L_i^T to high
        ///        relative accuracy by the dqds algorithm,
        ///    (c) If there is a cluster of close eigenvalues, &quot;choose&quot; sigma_i
        ///        close to the cluster, and go to step (a),
        ///    (d) Given the approximate eigenvalue lambda_j of L_i D_i L_i^T,
        ///        compute the corresponding eigenvector by forming a
        ///        rank-revealing twisted factorization.
        /// The desired accuracy of the output can be specified by the input
        /// parameter ABSTOL.
        /// </para>
        /// <para>
        /// For more details, see &quot;A new O(n^2) algorithm for the symmetric
        /// tridiagonal eigenvalue/eigenvector problem&quot;, by Inderjit Dhillon,
        /// Computer Science Division Technical Report No. UCB//CSD-97-971,
        /// UC Berkeley, May 1997.
        /// </para>
        /// <para>
        /// Note 1 : SSTEVR calls SSTEMR when the full spectrum is requested
        /// on machines which conform to the ieee-754 floating point standard.
        /// SSTEVR calls SSTEBZ and SSTEIN on non-ieee machines and
        /// when partial spectrum requests are made.
        /// </para>
        /// <para>
        /// Normal execution of SSTEMR may create NaNs and infinities and
        /// hence may abort due to a floating point exception in environments
        /// which do not handle NaNs and infinities in the ieee standard default
        /// manner.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="jobz">
        /// [in] JOBZ is CHARACTER*1.
        /// = &#39;N&#39;:  Compute eigenvalues only;
        /// = &#39;V&#39;:  Compute eigenvalues and eigenvectors.
        /// </param>
        /// <param name="range">
        /// [in] RANGE is CHARACTER*1.
        /// = &#39;A&#39;: all eigenvalues will be found.
        /// = &#39;V&#39;: all eigenvalues in the half-open interval (VL,VU]
        /// will be found.
        /// = &#39;I&#39;: the IL-th through IU-th eigenvalues will be found.
        /// For RANGE = &#39;V&#39; or &#39;I&#39; and IU - IL &lt; N - 1, SSTEBZ and
        /// SSTEIN are called
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix.  N &gt;= 0.
        /// </param>
        /// <param name="d">
        /// [in,out] D is REAL array, dimension (N).
        /// On entry, the n diagonal elements of the tridiagonal matrix
        /// A.
        /// On exit, D may be multiplied by a constant factor chosen
        /// to avoid over/underflow in computing the eigenvalues.
        /// </param>
        /// <param name="e">
        /// [in,out] E is REAL array, dimension (max(1,N-1)).
        /// On entry, the (n-1) subdiagonal elements of the tridiagonal
        /// matrix A in elements 1 to N-1 of E.
        /// On exit, E may be multiplied by a constant factor chosen
        /// to avoid over/underflow in computing the eigenvalues.
        /// </param>
        /// <param name="vl">
        /// [in] VL is REAL.
        /// If RANGE=&#39;V&#39;, the lower bound of the interval to
        /// be searched for eigenvalues. VL &lt; VU.
        /// Not referenced if RANGE = &#39;A&#39; or &#39;I&#39;.
        /// </param>
        /// <param name="vu">
        /// [in] VU is REAL.
        /// If RANGE=&#39;V&#39;, the upper bound of the interval to
        /// be searched for eigenvalues. VL &lt; VU.
        /// Not referenced if RANGE = &#39;A&#39; or &#39;I&#39;.
        /// </param>
        /// <param name="il">
        /// [in] IL is INTEGER.
        /// If RANGE=&#39;I&#39;, the index of the
        /// smallest eigenvalue to be returned.
        /// 1 &lt;= IL &lt;= IU &lt;= N, if N &gt; 0; IL = 1 and IU = 0 if N = 0.
        /// Not referenced if RANGE = &#39;A&#39; or &#39;V&#39;.
        /// </param>
        /// <param name="iu">
        /// [in] IU is INTEGER.
        /// If RANGE=&#39;I&#39;, the index of the
        /// largest eigenvalue to be returned.
        /// 1 &lt;= IL &lt;= IU &lt;= N, if N &gt; 0; IL = 1 and IU = 0 if N = 0.
        /// Not referenced if RANGE = &#39;A&#39; or &#39;V&#39;.
        /// </param>
        /// <param name="abstol">
        /// [in] ABSTOL is REAL.
        /// The absolute error tolerance for the eigenvalues.
        /// An approximate eigenvalue is accepted as converged
        /// when it is determined to lie in an interval [a,b]
        /// of width less than or equal to
        /// 
        /// ABSTOL + EPS *   max( |a|,|b| ) ,
        /// 
        /// where EPS is the machine precision.  If ABSTOL is less than
        /// or equal to zero, then  EPS*|T|  will be used in its place,
        /// where |T| is the 1-norm of the tridiagonal matrix obtained
        /// by reducing A to tridiagonal form.
        /// 
        /// See &quot;Computing Small Singular Values of Bidiagonal Matrices
        /// with Guaranteed High Relative Accuracy,&quot; by Demmel and
        /// Kahan, LAPACK Working Note #3.
        /// 
        /// If high relative accuracy is important, set ABSTOL to
        /// SLAMCH( &#39;Safe minimum&#39; ).  Doing so will guarantee that
        /// eigenvalues are computed to high relative accuracy when
        /// possible in future releases.  The current code does not
        /// make any guarantees about high relative accuracy, but
        /// future releases will. See J. Barlow and J. Demmel,
        /// &quot;Computing Accurate Eigensystems of Scaled Diagonally
        /// Dominant Matrices&quot;, LAPACK Working Note #7, for a discussion
        /// of which matrices define their eigenvalues to high relative
        /// accuracy.
        /// </param>
        /// <param name="m">
        /// [out] M is INTEGER.
        /// The total number of eigenvalues found.  0 &lt;= M &lt;= N.
        /// If RANGE = &#39;A&#39;, M = N, and if RANGE = &#39;I&#39;, M = IU-IL+1.
        /// </param>
        /// <param name="w">
        /// [out] W is REAL array, dimension (N).
        /// The first M elements contain the selected eigenvalues in
        /// ascending order.
        /// </param>
        /// <param name="z">
        /// [out] Z is REAL array, dimension (LDZ, max(1,M) ).
        /// If JOBZ = &#39;V&#39;, then if INFO = 0, the first M columns of Z
        /// contain the orthonormal eigenvectors of the matrix A
        /// corresponding to the selected eigenvalues, with the i-th
        /// column of Z holding the eigenvector associated with W(i).
        /// Note: the user must ensure that at least max(1,M) columns are
        /// supplied in the array Z; if RANGE = &#39;V&#39;, the exact value of M
        /// is not known in advance and an upper bound must be used.
        /// </param>
        /// <param name="ldz">
        /// [in] LDZ is INTEGER.
        /// The leading dimension of the array Z.  LDZ &gt;= 1, and if
        /// JOBZ = &#39;V&#39;, LDZ &gt;= max(1,N).
        /// </param>
        /// <param name="isuppz">
        /// [out] ISUPPZ is INTEGER array, dimension ( 2*max(1,M) ).
        /// The support of the eigenvectors in Z, i.e., the indices
        /// indicating the nonzero elements in Z. The i-th eigenvector
        /// is nonzero only in elements ISUPPZ( 2*i-1 ) through
        /// ISUPPZ( 2*i ).
        /// Implemented only for RANGE = &#39;A&#39; or &#39;I&#39; and IU - IL = N - 1
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  Internal error
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_sstevr", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Sstevr(
            MatrixLayout matrixLayout,
            char jobz,
            char range,
            int n,
            float* d,
            float* e,
            float vl,
            float vu,
            int il,
            int iu,
            float abstol,
            int* m,
            float* w,
            float* z,
            int ldz,
            int* isuppz);
    }
}
