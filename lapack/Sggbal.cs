using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// SGGBAL balances a pair of general real matrices (A,B).  This
        /// involves, first, permuting A and B by similarity transformations to
        /// isolate eigenvalues in the first 1 to ILO$-$1 and last IHI+1 to N
        /// elements on the diagonal; and second, applying a diagonal similarity
        /// transformation to rows and columns ILO to IHI to make the rows
        /// and columns as close in norm as possible. Both steps are optional.
        /// </para>
        /// <para>
        /// Balancing may reduce the 1-norm of the matrices, and improve the
        /// accuracy of the computed eigenvalues and/or eigenvectors in the
        /// generalized eigenvalue problem A*x = lambda*B*x.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="job">
        /// [in] JOB is CHARACTER*1.
        /// Specifies the operations to be performed on A and B:
        /// = &#39;N&#39;:  none:  simply set ILO = 1, IHI = N, LSCALE(I) = 1.0
        /// and RSCALE(I) = 1.0 for i = 1,...,N.
        /// = &#39;P&#39;:  permute only;
        /// = &#39;S&#39;:  scale only;
        /// = &#39;B&#39;:  both permute and scale.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrices A and B.  N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is REAL array, dimension (LDA,N).
        /// On entry, the input matrix A.
        /// On exit,  A is overwritten by the balanced matrix.
        /// If JOB = &#39;N&#39;, A is not referenced.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A. LDA &gt;= max(1,N).
        /// </param>
        /// <param name="b">
        /// [in,out] B is REAL array, dimension (LDB,N).
        /// On entry, the input matrix B.
        /// On exit,  B is overwritten by the balanced matrix.
        /// If JOB = &#39;N&#39;, B is not referenced.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B. LDB &gt;= max(1,N).
        /// </param>
        /// <param name="ilo">
        /// [out] ILO is INTEGER.
        /// </param>
        /// <param name="ihi">
        /// [out] IHI is INTEGER.
        /// ILO and IHI are set to integers such that on exit
        /// A(i,j) = 0 and B(i,j) = 0 if i &gt; j and
        /// j = 1,...,ILO-1 or i = IHI+1,...,N.
        /// If JOB = &#39;N&#39; or &#39;S&#39;, ILO = 1 and IHI = N.
        /// </param>
        /// <param name="lscale">
        /// [out] LSCALE is REAL array, dimension (N).
        /// Details of the permutations and scaling factors applied
        /// to the left side of A and B.  If P(j) is the index of the
        /// row interchanged with row j, and D(j)
        /// is the scaling factor applied to row j, then
        /// LSCALE(j) = P(j)    for J = 1,...,ILO-1
        /// = D(j)    for J = ILO,...,IHI
        /// = P(j)    for J = IHI+1,...,N.
        /// The order in which the interchanges are made is N to IHI+1,
        /// then 1 to ILO-1.
        /// </param>
        /// <param name="rscale">
        /// [out] RSCALE is REAL array, dimension (N).
        /// Details of the permutations and scaling factors applied
        /// to the right side of A and B.  If P(j) is the index of the
        /// column interchanged with column j, and D(j)
        /// is the scaling factor applied to column j, then
        /// LSCALE(j) = P(j)    for J = 1,...,ILO-1
        /// = D(j)    for J = ILO,...,IHI
        /// = P(j)    for J = IHI+1,...,N.
        /// The order in which the interchanges are made is N to IHI+1,
        /// then 1 to ILO-1.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value.
        /// </returns>
        /// <remarks>
        /// <para>
        ///  See R.C. WARD, Balancing the generalized eigenvalue problem,
        ///                 SIAM J. Sci. Stat. Comp. 2 (1981), 141-152.
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_sggbal", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Sggbal(
            MatrixLayout matrixLayout,
            char job,
            int n,
            float* a,
            int lda,
            float* b,
            int ldb,
            int* ilo,
            int* ihi,
            float* lscale,
            float* rscale);
    }
}
