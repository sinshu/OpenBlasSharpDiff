using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DGEBAL balances a general real matrix A.  This involves, first,
        /// permuting A by a similarity transformation to isolate eigenvalues
        /// in the first 1 to ILO-1 and last IHI+1 to N elements on the
        /// diagonal; and second, applying a diagonal similarity transformation
        /// to rows and columns ILO to IHI to make the rows and columns as
        /// close in norm as possible.  Both steps are optional.
        /// </para>
        /// <para>
        /// Balancing may reduce the 1-norm of the matrix, and improve the
        /// accuracy of the computed eigenvalues and/or eigenvectors.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="job">
        /// [in] JOB is CHARACTER*1.
        /// Specifies the operations to be performed on A:
        /// = &#39;N&#39;:  none:  simply set ILO = 1, IHI = N, SCALE(I) = 1.0
        /// for i = 1,...,N;
        /// = &#39;P&#39;:  permute only;
        /// = &#39;S&#39;:  scale only;
        /// = &#39;B&#39;:  both permute and scale.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is DOUBLE PRECISION array, dimension (LDA,N).
        /// On entry, the input matrix A.
        /// On exit,  A is overwritten by the balanced matrix.
        /// If JOB = &#39;N&#39;, A is not referenced.
        /// See Further Details.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="ilo">
        /// [out] ILO is INTEGER.
        /// </param>
        /// <param name="ihi">
        /// [out] IHI is INTEGER.
        /// ILO and IHI are set to integers such that on exit
        /// A(i,j) = 0 if i &gt; j and j = 1,...,ILO-1 or I = IHI+1,...,N.
        /// If JOB = &#39;N&#39; or &#39;S&#39;, ILO = 1 and IHI = N.
        /// </param>
        /// <param name="scale">
        /// [out] SCALE is DOUBLE PRECISION array, dimension (N).
        /// Details of the permutations and scaling factors applied to
        /// A.  If P(j) is the index of the row and column interchanged
        /// with row and column j and D(j) is the scaling factor
        /// applied to row and column j, then
        /// SCALE(j) = P(j)    for j = 1,...,ILO-1
        /// = D(j)    for j = ILO,...,IHI
        /// = P(j)    for j = IHI+1,...,N.
        /// The order in which the interchanges are made is N to IHI+1,
        /// then 1 to ILO-1.
        /// </param>
        /// <returns>
        /// = 0:  successful exit.
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value.
        /// </returns>
        /// <remarks>
        /// <para>
        ///  The permutations consist of row and column interchanges which put
        ///  the matrix in the form
        /// </para>
        /// <para>
        ///             ( T1   X   Y  )
        ///     P A P = (  0   B   Z  )
        ///             (  0   0   T2 )
        /// </para>
        /// <para>
        ///  where T1 and T2 are upper triangular matrices whose eigenvalues lie
        ///  along the diagonal.  The column indices ILO and IHI mark the starting
        ///  and ending columns of the submatrix B. Balancing consists of applying
        ///  a diagonal similarity transformation inv(D) * B * D to make the
        ///  1-norms of each row of B and its corresponding column nearly equal.
        ///  The output matrix is
        /// </para>
        /// <para>
        ///     ( T1     X*D          Y    )
        ///     (  0  inv(D)*B*D  inv(D)*Z ).
        ///     (  0      0           T2   )
        /// </para>
        /// <para>
        ///  Information about the permutations P and the diagonal matrix D is
        ///  returned in the vector SCALE.
        /// </para>
        /// <para>
        ///  This subroutine is based on the EISPACK routine BALANC.
        /// </para>
        /// <para>
        ///  Modified by Tzu-Yi Chen, Computer Science Division, University of
        ///    California at Berkeley, USA
        /// </para>
        /// <para>
        ///  Refactored by Evert Provoost, Department of Computer Science,
        ///    KU Leuven, Belgium
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dgebal", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dgebal(
            MatrixLayout matrixLayout,
            char job,
            int n,
            double* a,
            int lda,
            int* ilo,
            int* ihi,
            double* scale);
    }
}
