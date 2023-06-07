use crate::structs::*;

pub fn optimize(e: Expr) -> Expr {
    let mut olde = e;
    let mut newe = optimize_recurse(&olde);
    while newe != olde {
        olde = newe;
        newe = optimize_recurse(&olde);
    }
    newe.clone()
}

fn optimize_recurse(e: &Expr) -> Expr {
    
    e.clone()
}
