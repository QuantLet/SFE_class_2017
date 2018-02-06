function result = normt_rnd(mu,sigma2,left,right)

    stderrs = sqrt(sigma2);

    points_left = find(left==-999);
    points_right = find(right==999);

    a_term = normcdf( (left-mu)./stderrs);
    a_term(points_left) = 0;

    b_term = normcdf( (right-mu)./stderrs);
    b_term(points_right) = 1;

    uniforms = rand(length(mu),1);

    p = a_term + uniforms.*(b_term - a_term);

    result = mu + stderrs.*norminv(p);
end