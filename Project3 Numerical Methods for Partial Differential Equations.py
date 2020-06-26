from numpy import *
from numpy.polynomial import legendre

from __future__ import division, print_function

from scipy.sparse import *
import scipy.sparse.linalg as splinalg

------------------------- 'variables setting' -------------------------------


    '''
    Cauchy-Green finite strain tensor field.
    '''
def _Axx(x, y):
    return ravel(transpose([_ddx(x, y), zeros(4)]))

def _Ayy(x, y):
    return ravel(transpose([zeros(4), _ddy(x, y)]))

def _Axy(x, y):
    return ravel(transpose([_ddy(x, y), _ddx(x, y)]))


def _shpfn(x, y):
    return array([(1-x) * (1-y), (1+x) * (1-y), (1+x) * (1+y), (1-x) * (1+y)]) / 4

def _ddy(x, y):
    return array([-(1-x), -(1+x), (1+x), (1-x)]) / 4

def _ddx(x, y):
    return array([-(1-y), (1-y), (1+y), -(1+y)]) / 4




----------------------------------------------------------------------------------

    '''
    elastic energy; Quadratic and cubic components
    '''
def stiff_mat(nu):
    n = 4
    x_g, w_g = legendre.leggauss(n)
    x_g, y_g = meshgrid(x_g, x_g)
    w_g = outer(w_g, w_g)
    q0 = zeros([2 * n, 2 * n])
    q1 = zeros([2 * n, 2 * n])
    for i in range(n):
        for j in range(n):
            axx = _Axx(x_g[i,j], y_g[i,j])
            ayy = _Ayy(x_g[i,j], y_g[i,j])
            axy = _Axy(x_g[i,j], y_g[i,j])
            q0 += w_g[i,j] * ((1/2) * outer(axy, axy) + outer(axx, axx) + outer(ayy, ayy))
            q1 += w_g[i,j] * (-(1/2) * outer(axy, axy) + outer(axx, ayy) + outer(ayy, axx))
    premul = 1 / (1 - nu**2)
    return premul * (nu * q1 + q0)

class fm2D:
    
    def xy(self):
        x, y = meshgrid(arange(self.nel_x+1), self.nel_y - arange(self.nel_y+1), indexing='ij')
        return 2 * self.size * array([x, y])


    def K_matrix(self, E):
        assert E.shape == (self.nel_x, self.nel_y)
        K = csr_matrix((kron(ravel(E), ravel(self.Q)), (self._i_K, self._j_K)))
        return (K + K.T) / 2

     '''
     nu = Poisson ratio
     shape = (nel_x,nel_y)
     '''

    def __init__(self, nu, nel_x, nel_y, size):
        self.Q = stiff_mat(nu)
        self.nel_x, self.nel_y = nel_x, nel_y
        self.size = size
        self.edofMat = edofVec[:,newaxis] + \
                ravel(array([1, nel_y+2, nel_y+1, 0])[:,newaxis] * 2 + arange(2))
        self._i_K = ravel(kron(self.edofMat, ones([8,1], dtype=int)))
        self._j_K = ravel(kron(self.edofMat, ones([1,8], dtype=int)))
        nod = reshape(arange((1+nel_x)*(1+nel_y)), [1+nel_x,1+nel_y])
        edofVec = ravel(2 * nod[:-1,:-1])
       
        



------------------------------ 'base functions' ----------------------------
def node_x(i, j, nel_x, nel_y):
    if i < 0: i = i +nel_x + 1 
    if j < 0: j = j +nel_y + 1 
    return 2 * (i * (nel_y + 1) + j) 

def node_y(i, j, nel_x, nel_y):
    return node_x(i, j, nel_x, nel_y) + 1

def plot_deformation(fm, U):
    nel_x, nel_y = fm.nel_x, fm.nel_y
    U = U.reshape([nel_x+1, nel_y+1, 2])
    x, y = fm.xy
    dx = U[:,:,0]
    dy = U[:,:,1]
    py_lab.plot(x, y, 'k', lw=.5)
    py_lab.plot(x.T, y.T, 'k', lw=.5)
    py_lab.plot(x+dx, y+dy, 'r', lw=.5)
    py_lab.plot((x+dx).T, (y+dy).T, 'r', lw=.5)
    py_lab.axis('sized')
    margin = max(x.max() - x.min(), y.max() - y.min()) * 0.2
    py_lab.xlim([-x.min() - margin, x.max() + margin])
    py_lab.ylim([-x.min() - margin, y.max() + margin])

--------------------------------- 'Answers' --------------------------------

E0 = 1.18E11
nu = 0.31

def compute_deformation(nel_x, nel_y, size=1):
    E = E0 * ones([nel_x, nel_y])

    F = zeros(2*(nel_y+1)*(nel_x+1))
    if nel_y % 2 == 0:
        F[node_y(-1, nel_y//2, nel_x, nel_y)] = -8E4
    else:
        F[node_y(-1, nel_y//2, nel_x, nel_y)] = -4E4
        F[node_y(-1, nel_y//2+1, nel_x, nel_y)] = -4E4

    fixeddofs = set([node_x(0, j, nel_x, nel_y) for j in range(nel_y+1)]
                  + [node_y(0, j, nel_x, nel_y) for j in range(nel_y+1)])
    alldofs = set(arange(F.size))
    freedofs = array(sorted(set.difference(alldofs, fixeddofs)), int)

    fm = fm2D(nu, nel_x, nel_y, size)
    K = fm.K_matrix(E)
    U = zeros_l_i_Ke(F)
    U[freedofs] = splinalg.spsolve(K[freedofs,:][:,freedofs], F[freedofs])
    return fm, U.reshape([nel_x + 1, nel_y + 1, 2])

def q_2a():
    fm, U = compute_deformation(1, 1)
    py_lab.figure()
    plot_deformation(fm, U * 5E4)
    title('Deformation exaggerating with factor of 50,000')

plot_exaggeration = {2:20000, 4:5000, 10:1000}

def q_2b(N):
    fm, U = compute_deformation(N, 1)
    py_lab.figure()
    exagg = plot_exaggeration[N]
    plot_deformation(fm, U * exagg)
    title('Deformation exaggerating with a factor of {}'.format(exagg))

if __name__ == '__main__':
    import py_lab
    q_2a()
    for N in [2,4,10]:
        q_2b(N)
