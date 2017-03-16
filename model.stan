data{
    int<lower=1> N;
    int obs[N];
}
parameters{
    ordered[4] cutpoints;
}
model{
    vector[N] phi;
    cutpoints ~ normal( 0 , 10 );
    for ( i in 1:N ) {
        phi[i] = 0;
    }
    for ( i in 1:N )
    obs[i] ~ ordered_logistic( phi[i] , cutpoints );
}
generated quantities{
    vector[N] phi;
    real dev;
    dev = 0;
    for ( i in 1:N ) {
        phi[i] = 0;
    }
    for ( i in 1:N )
    dev = dev + (-2)*ordered_logistic_lpmf( obs[i] | phi[i] , cutpoints );
}
