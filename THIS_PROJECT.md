# Carbon Policies in Production Networks

**Authors:** Gabriel Jardanovski & Gert Bijnens

## Research Question

Carbon pricing policies (CPP) are increasingly used to fight climate change, but in practice they never cover all firms or sectors. Policymakers target a subset of polluters. This project asks:

**How do targeted carbon pricing policies affect aggregate emissions, and what role does the production network play?**

We show that the input-output structure can either amplify or dampen the direct abatement effects of a targeted CPP. The direction depends on whether the firms whose prices rise the most (due to direct taxation and network propagation) are also the firms with the highest network-adjusted emission intensity. We derive a sufficient statistic that captures this interaction and measure it using Belgian micro-data.

## Approach

### 1. Theory

We build a model of a closed economy with firms connected through I-O linkages where:

- Firms have heterogeneous emission intensities (across and within sectors) and can invest in abatement.
- The government sets a carbon price and a targeting vector (which firms face the tax).
- Prices equal marginal cost (full pass-through), so the carbon tax propagates through the network.

We decompose the change in aggregate emissions into abatement (technique) and reallocation (composition) channels and show that, in a CES economy, the reallocation channel is governed by a sufficient statistic:

$$\sum_i \frac{x_i}{Z} \mathbb{C}_{\Omega_{(i,:)}} \big( \text{Exposure}_i, \text{Network-adj. EI}_i \big)$$

This is the input-weighted covariance between a firm's exposure to the policy and its network-adjusted emission intensity. The sign determines whether the I-O structure amplifies (positive) or dampens (negative) the abatement effects.

Three mechanisms feed into this statistic:
1. Substitution toward untargeted competitors (leakage)
2. Emissions embodied in supply chains
3. Substitution occurring at different layers of the network

We also show that different targeting schemes (which firms are taxed) lead to different values of this statistic, so that some targeting schemes reduce aggregate emissions more than others for the same carbon price.

### 2. Empirical measurement (Belgium)

We use Belgian micro-data to measure the sufficient statistic:
- **EUTL**: installation-level emissions for EUETS-targeted firms (2005-2022)
- **Imputation**: sector-level emissions from energy balances + national inventories, distributed to non-targeted firms proportional to firm size
- **NBB firm-level data**: B2B transactions and balance sheets, yielding a firm-year-level I-O matrix for the Belgian private sector

Preliminary evidence suggests the Belgian I-O structure amplifies the abatement effects of the EUETS targeting.

### 3. Quantitative analysis

We discipline the elasticity of substitution across intermediate inputs and use the model to run counterfactual exercises:
1. What were the aggregate effects of the current EU ETS targeting on Belgian emissions 2005-2020, decomposed into scale/technique/composition?
2. What if all industrial emissions had been targeted from day 1?
3. What if we kept the same % coverage but targeted firms with the highest network centrality?
4. What if targeting is restricted to the sector level (all-or-nothing by sector)?

We also characterize which firms have the highest network centrality by regressing the centrality measure against observables available to policymakers (size, emissions, emission intensity, upstream emissions, sector).

## Paper Structure

The paper (`paper/main.tex`) is organized as:
1. **Introduction** (`sections/introduction.tex`)
2. **Carbon Policy and Network Structure** (`sections/model.tex`) — environment, non-parametric decomposition, CES sufficient statistic, network centrality measure
3. **Data and Descriptive Evidence** (`sections/data.tex`) — EU ETS context, Belgian coverage, data sources, imputation, descriptive facts
4. **Quantitative Analysis** (`sections/quantitative.tex`) — elasticity estimation, centrality characterization, counterfactuals
5. **Conclusion** (`sections/conclusion.tex`)
6. **Appendix** (`sections/appendix.tex`) — proofs, data appendix, covariance operator definition
