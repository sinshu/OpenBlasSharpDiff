using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        ///  ZUNHR_COL takes an M-by-N complex matrix Q_in with orthonormal columns
        ///  as input, stored in A, and performs Householder Reconstruction (HR),
        ///  i.e. reconstructs Householder vectors V(i) implicitly representing
        ///  another M-by-N matrix Q_out, with the property that Q_in = Q_out*S,
        ///  where S is an N-by-N diagonal matrix with diagonal entries
        ///  equal to +1 or -1. The Householder vectors (columns V(i) of V) are
        ///  stored in A on output, and the diagonal entries of S are stored in D.
        ///  Block reflectors are also returned in T
        ///  (same output format as ZGEQRT).
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of rows of the matrix A. M &gt;= 0.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of columns of the matrix A. M &gt;= N &gt;= 0.
        /// </param>
        /// <param name="nb">
        /// [in] NB is INTEGER.
        /// The column block size to be used in the reconstruction
        /// of Householder column vector blocks in the array A and
        /// corresponding block reflectors in the array T. NB &gt;= 1.
        /// (Note that if NB &gt; N, then N is used instead of NB
        /// as the column block size.)
        /// </param>
        /// <param name="a">
        /// [in,out] A is COMPLEX*16 array, dimension (LDA,N).
        /// 
        /// On entry:
        /// 
        /// The array A contains an M-by-N orthonormal matrix Q_in,
        /// i.e the columns of A are orthogonal unit vectors.
        /// 
        /// On exit:
        /// 
        /// The elements below the diagonal of A represent the unit
        /// lower-trapezoidal matrix V of Householder column vectors
        /// V(i). The unit diagonal entries of V are not stored
        /// (same format as the output below the diagonal in A from
        /// ZGEQRT). The matrix T and the matrix V stored on output
        /// in A implicitly define Q_out.
        /// 
        /// The elements above the diagonal contain the factor U
        /// of the &quot;modified&quot; LU-decomposition:
        /// Q_in - ( S ) = V * U
        /// ( 0 )
        /// where 0 is a (M-N)-by-(M-N) zero matrix.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,M).
        /// </param>
        /// <param name="t">
        /// [out] T is COMPLEX*16 array,.
        /// dimension (LDT, N)
        /// 
        /// Let NOCB = Number_of_output_col_blocks
        /// = CEIL(N/NB)
        /// 
        /// On exit, T(1:NB, 1:N) contains NOCB upper-triangular
        /// block reflectors used to define Q_out stored in compact
        /// form as a sequence of upper-triangular NB-by-NB column
        /// blocks (same format as the output T in ZGEQRT).
        /// The matrix T and the matrix V stored on output in A
        /// implicitly define Q_out. NOTE: The lower triangles
        /// below the upper-triangular blocks will be filled with
        /// zeros. See Further Details.
        /// </param>
        /// <param name="ldt">
        /// [in] LDT is INTEGER.
        /// The leading dimension of the array T.
        /// LDT &gt;= max(1,min(NB,N)).
        /// </param>
        /// <param name="d">
        /// [out] D is COMPLEX*16 array, dimension min(M,N).
        /// The elements can be only plus or minus one.
        /// 
        /// D(i) is constructed as D(i) = -SIGN(Q_in_i(i,i)), where
        /// 1 &lt;= i &lt;= min(M,N), and Q_in_i is Q_in after performing
        /// i-1 steps of “modified” Gaussian elimination.
        /// See Further Details.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        /// <remarks>
        /// <para>
        /// The computed M-by-M unitary factor Q_out is defined implicitly as
        /// a product of unitary matrices Q_out(i). Each Q_out(i) is stored in
        /// the compact WY-representation format in the corresponding blocks of
        /// matrices V (stored in A) and T.
        /// </para>
        /// <para>
        /// The M-by-N unit lower-trapezoidal matrix V stored in the M-by-N
        /// matrix A contains the column vectors V(i) in NB-size column
        /// blocks VB(j). For example, VB(1) contains the columns
        /// V(1), V(2), ... V(NB). NOTE: The unit entries on
        /// the diagonal of Y are not stored in A.
        /// </para>
        /// <para>
        /// The number of column blocks is
        /// </para>
        /// <para>
        ///     NOCB = Number_of_output_col_blocks = CEIL(N/NB)
        /// </para>
        /// <para>
        /// where each block is of order NB except for the last block, which
        /// is of order LAST_NB = N - (NOCB-1)*NB.
        /// </para>
        /// <para>
        /// For example, if M=6,  N=5 and NB=2, the matrix V is
        /// </para>
        /// <para>
        ///     V = (    VB(1),   VB(2), VB(3) ) =
        /// </para>
        /// <para>
        ///       = (   1                      )
        ///         ( v21    1                 )
        ///         ( v31  v32    1            )
        ///         ( v41  v42  v43   1        )
        ///         ( v51  v52  v53  v54    1  )
        ///         ( v61  v62  v63  v54   v65 )
        /// </para>
        /// <para>
        /// For each of the column blocks VB(i), an upper-triangular block
        /// reflector TB(i) is computed. These blocks are stored as
        /// a sequence of upper-triangular column blocks in the NB-by-N
        /// matrix T. The size of each TB(i) block is NB-by-NB, except
        /// for the last block, whose size is LAST_NB-by-LAST_NB.
        /// </para>
        /// <para>
        /// For example, if M=6,  N=5 and NB=2, the matrix T is
        /// </para>
        /// <para>
        ///     T  = (    TB(1),    TB(2), TB(3) ) =
        /// </para>
        /// <para>
        ///        = ( t11  t12  t13  t14   t15  )
        ///          (      t22       t24        )
        /// </para>
        /// <para>
        /// The M-by-M factor Q_out is given as a product of NOCB
        /// unitary M-by-M matrices Q_out(i).
        /// </para>
        /// <para>
        ///     Q_out = Q_out(1) * Q_out(2) * ... * Q_out(NOCB),
        /// </para>
        /// <para>
        /// where each matrix Q_out(i) is given by the WY-representation
        /// using corresponding blocks from the matrices V and T:
        /// </para>
        /// <para>
        ///     Q_out(i) = I - VB(i) * TB(i) * (VB(i))**T,
        /// </para>
        /// <para>
        /// where I is the identity matrix. Here is the formula with matrix
        /// dimensions:
        /// </para>
        /// <para>
        ///  Q(i){M-by-M} = I{M-by-M} -
        ///    VB(i){M-by-INB} * TB(i){INB-by-INB} * (VB(i))**T {INB-by-M},
        /// </para>
        /// <para>
        /// where INB = NB, except for the last block NOCB
        /// for which INB=LAST_NB.
        /// </para>
        /// <para>
        /// =====
        /// NOTE:
        /// =====
        /// </para>
        /// <para>
        /// If Q_in is the result of doing a QR factorization
        /// B = Q_in * R_in, then:
        /// </para>
        /// <para>
        /// B = (Q_out*S) * R_in = Q_out * (S * R_in) = Q_out * R_out.
        /// </para>
        /// <para>
        /// So if one wants to interpret Q_out as the result
        /// of the QR factorization of B, then the corresponding R_out
        /// should be equal to R_out = S * R_in, i.e. some rows of R_in
        /// should be multiplied by -1.
        /// </para>
        /// <para>
        /// For the details of the algorithm, see [1].
        /// </para>
        /// <para>
        /// [1] &quot;Reconstructing Householder vectors from tall-skinny QR&quot;,
        ///     G. Ballard, J. Demmel, L. Grigori, M. Jacquelin, H.D. Nguyen,
        ///     E. Solomonik, J. Parallel Distrib. Comput.,
        ///     vol. 85, pp. 3-31, 2015.
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zunhr_col", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo ZunhrCol(
            MatrixLayout matrixLayout,
            int m,
            int n,
            int nb,
            Complex* a,
            int lda,
            Complex* t,
            int ldt,
            Complex* d);
    }
}
